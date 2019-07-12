load_safe_data <- function(record_id, worksheet){
	
	#' Loads data from a SAFE dataset.
	#'
	#' This function returns a data frame containing the data from a data 
	#' worksheet in a SAFE dataset. Note that SAFE dataset .xlsx files include
	#' the other (non-data) worksheets Summary, Taxa, Locations that contain 
	#' metadata: see  \code{get_taxa}, \code{get_locations}, \code{add_taxa} 
	#' and \code{add_locations} for accessing and using this metadata.
	#' 
	#' In particular, the large amount of data worksheet summary metadata is
	#' not attached as attributes to the data frame returned by this function.
	#' This is largely to avoid spamming the screen during normal use of the
	#' data frame: an extended description of a worksheet can be displayed using
	#' \code{show_worksheet}.
	#'
	#' Currently, this function only loads data from SAFE formatted
	#' .xlsx files - data stored in external files is not yet handled.
	#'
	#' @param record_id A SAFE dataset record id 
	#' @param name The name of the worksheet to load
	#' @return A data frame with the additional 'safedata' class
	#' @export

	# validate the record id
	record_set <- validate_record_ids(record_id)

	if((nrow(record_set) != 1) | is.na(record_set$record)){
		stop("record_id does not consist of a single record version id")
	} else if (! record_set$available){
		stop("The record is under embargo or restricted")
	}
	
	safedir <- get_data_dir()
	index <- get_index()
	
	# Now get the metadata and find the target worksheet
	metadata <- load_record_metadata(record_set)
	if(! worksheet %in% metadata$metadata$dataworksheets$name){
		stop('Unknown data worksheet name')
	}

	dwksh <- metadata$metadata$dataworksheets[metadata$metadata$dataworksheets$name == worksheet, ]
	
	# Look for a local copy of the file. If it doesn't exist, download it if possible
	row <- subset(index, zenodo_record_id == record_set$record)
	local_path <- file.path(safedir, row$zenodo_concept_id, row$zenodo_record_id, row$filename)
	
	if(! file.exists(local_path)){
		verbose_message('Downloading datafile: ', row$filename)
		download_safe_files(record_set)
	}
	
	# Validate the local copy
	local_md5 <- tools::md5sum(path.expand(local_path))
	if(local_md5 != row$checksum){
		stop('Local file has been modified - do not edit files within the SAFE data directory')
	}
	
	# Now load the data - using readxl, openxlsx is also possible but seems
	# to be orphaned and has some date time handling issues, but don't use tibbles
	data <- readxl::read_xlsx(local_path, worksheet, 
							  skip = dwksh$field_name_row - 1, 
							  n_max = dwksh$n_data_row, na='NA')
							  
	# drop tibble class and first column of row numbers
	class(data) <- 'data.frame'
	data <- data[,-1]
	
	# Now do field type conversions
	fields <- dwksh$fields[[1]]
	
	if(! all.equal(fields$field_name, names(data))){
		stop('Mismatch between data field names and local metadata')
	}
	
	for(idx in seq_along(names(data))){
		
		fld <- fields[idx, ]
		
		# Factors
		if(grepl('Categorical', fld$field_type)){
			data[fld$field_name] <- as.factor(data[[fld$field_name]])
			
		}
		
		# Dates, Datetimes and Times 
		if(fld$field_type %in% c('Date','Datetime', 'Time')){
			
			values <- data[[fld$field_name]]
			# if they haven't been converted already, then the user has supplied
			# POSIX strings not Excel Date/Time
			if(! inherits(values, 'POSIXt')){
				values <- try(as.POSIXct(values, tryFormats = 
										 c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M",
										   "%Y-%m-%d", "%H:%M:%OS", "%H:%M")))
				if(inherits(values, 'try-error')){
					stop('Failed to convert date/times in field ', fld$field_name)
				}
			}
			# Use chron time - could use chron date + chron but they provide a 
			# ugly pile of attributes in the data frame display and have to be 
			# beaten with a stick to stop them using month/day/year formats.
			
			if(fld$field_type == 'Time'){
				values <- chron::times(format(values, '%H:%M:%S'))
			}
			data[fld$field_name] <- values
		}
	}

	class(data) <- c('data.frame', 'safe_data')
	attr(data, 'safe_data') <- list(safe_record_set=record_set, worksheet=worksheet)
	return(data)
}


str.safe_data <- function(object, ...){
	
	#' @describeIn load_safe_data Display structure of a safedata data frame
	#'
	#' 
	#' @method str safedata
	
	object_attr <- attr(object, 'safe_data')
	with(object_attr, cat(sprintf('SAFE Concept: %i; SAFE Record %i; Worksheet: %s\n', 
								  safe_record_set$concept, safe_record_set$record, worksheet)))
	
	# reduce the safedata object to a simple data frame and recall str(x, ...)
	attributes(object) <- NULL
	class(object_attr) <- 'data.frame'
	invisible(str.data.frame(object_attr, ...))

}


download_safe_files <- function(record_ids, confirm=TRUE, xlsx_only=TRUE, 
							    download_metadata=TRUE, refresh=FALSE, token=NULL){
	
	#' Download SAFE dataset files
	#'
	#' This downloads files associated with SAFE datasets, either all of the files 
	#' included in a set of records (\code{xlsx_only=FALSE}) or just the core .xlsx
	#' files (\code{xlsx_only=FALSE}), and stores them in the SAFE data directory.
	#' Currently, there is no mechanism for importing restricted datasets. 
	#'
	#' By default, the function will also download the dataset metadata. This 
	#' information is required by many of the functions in the package but users
	#' can turn off automatic metadata download.
	#'
	#' @section Warning:
	#' Using \code{refresh=TRUE} will \strong{overwrite locally modified files} and 
	#' replace them with the versions of record from Zenodo.
	#'
	#' @param record_ids A vector of SAFE dataset record ids 
	#' @param confirm Requires the user to confirm before download (logical)	
	#' @param xlsx_only Should all files be downloaded or just the core .xslx file (logical)
	#' @param download_metadata Should the metadata record for the file be downloaded (logical)
	#' @param refresh Should the function check if local copies have been modified and 
	#'   download fresh copies. This is useful if the local copies have unintentionally 
	#'   been modified but note the warning above.
	#' @param token An access token for restricted datasets. Not currently implemented.
	#' @export
	
	# validate the record ids
	record_set <- validate_record_ids(record_ids)
	
	records_to_get <- record_set$record[! is.na(record_set$record)]
	
	if(! length(records_to_get)){
		verbose_message('No valid record ids provided')
		return(invisible())
	} 
	
	# Get the target files
	index <- get_index()
	safedir <- get_data_dir()
	
	# Get the set of files
	if(xlsx_only){
		targets <- subset(index, zenodo_record_id %in% records_to_get & grepl('.xlsx$', filename))
	} else {
		targets <- subset(index, zenodo_record_id %in% records_to_get)
	}
		
	# See what is stored locally
	targets$local_path <- path.expand(file.path(safedir, targets$path))
	targets$local_exists <- file.exists(targets$local_path)
	
	# Check which files are already local and optionally which have bad MD5 sums
	if(refresh){
		targets$refresh <- targets$checksum != tools::md5sum(targets$local_path)	
	} else {
		targets$refresh <- FALSE
	}
	
	# Slightly naughtily using an unexported function call from utils
	msg <- paste0('%i files requested from %i records\n',
				  ' - %i local (%s)\n',
				  ' - %i embargoed or restricted (%s)\n',
				  ' - %i to download (%s)')

	local <- subset(targets, (! refresh) & local_exists)
	unavail <- subset(targets, ! available)
	to_download <- subset(targets, (refresh | (! local_exists)) & available)
	
	msg <- sprintf(msg, nrow(targets), length(unique(targets$zenodo_record_id)),
				   nrow(local), utils:::format.object_size(sum(local$filesize), "auto"),
				   nrow(unavail), utils:::format.object_size(sum(unavail$filesize), "auto"),
				   nrow(to_download), utils:::format.object_size(sum(to_download$filesize), "auto"))
	
	if(confirm){
		# Don't mute the message if the function is called to report this!
		confirm_response <- utils::menu(c('Yes', 'No'), title=msg)
		if(confirm_response != 1){
			message('Aborting download')
			return(invisible())
		}
	} else {
		verbose_message(msg)
	}
	
	# download metadata if requested
	if(download_metadata){
		fetch_record_metadata(record_set)
	}
	
	# split by records
	files_by_record <- split(targets, targets$zenodo_record_id)
	
	for(these_files in files_by_record){
		
		current_record <- these_files$zenodo_record_id[1]
		
		if(! these_files$available[1]){
			verbose_message(sprintf('%i files for record %i: under embargo or restricted', 
									nrow(these_files), current_record))
			next
		} 
			
		verbose_message(sprintf('%i files for record %i: %i to download', 
								nrow(these_files), current_record, 
								sum((! these_files$local_exists) | these_files$refresh)))
								
		these_files <- subset(these_files, (! local_exists) | refresh)

		if(nrow(these_files)){
		
			# For restricted datasets, users can request access via Zenodo and get a link 
			# with an access token but the token does not work with the Zenodo API and using 
			# it with the standard file URLs is not trivial - ? needs cookies
			# e.g. https://zenodo.org/records/315677/files/test.xlsx?download=1
	
			# If there are any files to download we need to get the remote URL from the 
			# Zenodo API - the 'bucket' id in the URLs is not persistent, so can't be indexed
			
			remote_url <- sprintf('https://zenodo.org/api/records/%i', current_record)
			zenodo_record <- try(jsonlite::fromJSON(remote_url), silent=TRUE)
		
			if(inherits(zenodo_record, 'try-error')){
				warning(' - Unable to retrieve remote file details for record ', current_record)
			} else {
		
				# Match zenodo files to local file list 
				zenodo_files <- data.frame(filename=zenodo_record$files$filename,
										   download=zenodo_record$files$links$download,
										   stringsAsFactors=FALSE)
		
				these_files <- merge(these_files, zenodo_files, by='filename', all.x=TRUE)
		
				if(any(is.na(these_files$download))){
					warning(' - Mismatch between local index and remote file details for record ', current_record)
				} else {
		
					# Now download the required files
					for(row_idx in seq_along(these_files$filename)){
			
						this_file <- these_files[row_idx,]
						
						# Look to see if the target directory exists.
						if(! file.exists(dirname(this_file$local_path))){
							dir.create(dirname(this_file$local_path), recursive=TRUE)
						}
						
						# Download the target file to the directory
						result <- with(this_file, try(curl::curl_download(download, dest=local_path), silent=TRUE))
		
						if(inherits(result, 'try-error')){
							if(this_file$local_exists){
								verbose_message(' - Failed to refresh: ', this_file$filename)
							} else {
								verbose_message(' - Failed to download: ', this_file$filename)
							}
						} else {
							if(this_file$local_exists){
								verbose_message(' - Refreshed: ', this_file$filename)
							} else {
								verbose_message(' - Downloaded: ', this_file$filename)
							}
						}
					}
				}
			}
		}
	}
}

zenodoRecordApiLookup <- function (id) {
  #' Return information of the given id using the Zenodo record API
  #' 
  #' This function returns a \code{list} object from an API call to the Zenodo
  #' cloud database using the supplied ID number. The \code{list} object
  #' contains varied record information for the ID.
  #' 
  #' @param id the data ID number, which in this instance should be either a
  #'   SAFE version DOI or concept DOI number
  #' @return \code{list} object containing information from the Zenodo API call
  #' @note Zenodo Record API: \url{https://zenodo.org/api/records/}
  
  return(jsonlite::fromJSON(RCurl::getURL(
    paste0('https://zenodo.org/api/records/', id))))
}

zenodoVersionsApiLookup <- function (id) {
  #' Perform a lookup on the Zenodo database with an API to access information
  #' for all versions of the specified id.
  #' 
  #' This function returns a \code{list} onject from an API call to the Zenodo
  #' cloud database using the supplied record ID number. The returned 
  #' \code{list}contains information on all available versions of the data
  #' assigned to that ID. Note that this API call only returns a populated 
  #' \code{list} if a concept record ID is passed, otherwise the \code{list} is
  #' empty.
  #' 
  #' @param id the data ID number, which in this instance should be a concept ID
  #' @return \code{list} object containing information on all versions of the ID
  #'   from the Zenodo API call
  #' @note Zenodo Versions API example: 
  #'   \url{https://zenodo.org/api/records/?q=conceptrecid:1198692&all_versions&sort=-version&size=10&page=1}
  
  return(jsonlite::fromJSON(RCurl::getURL(
    paste0('https://zenodo.org/api/records/?q=conceptrecid:', id, 
           '&all_versions&sort=-version&size=10&page=1'))))
}

isSafeRecord <- function (zenodoRecord) {
  #' Checks whether the supplied zenodo record is from the SAFE community
  #' 
  #' This function returns a boolean indicating whether the supplied Zenodo
  #' record information (the result of an API lookup to the Zenodo database) is 
  #' part of the SAFE (Stability of Altered Forest Ecosystems) community.
  #' 
  #' @param zenodoRecord returned \code{list} object from an API call to the
  #'   Zenodo cloud storage database. Should contain information pertaining to
  #'   a Zenodo record (or records)
  #' @return \code{TRUE}/\code{FALSE} indicating whether the record is part of
  #'   the SAFE community
  #' @seealso \url{https://zenodo.org/communities/safe/},
  #'   \code{\link{zenodoRecordApiLookup}}, \code{\link{zenodoVersionsApiLookup}}
  
  if (!is.null(zenodoRecord$metadata)) {
    # in this case a record ID has been passed
    if ('safe' %in% zenodoRecord$metadata$communities$id) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (!is.null(zenodoRecord$hits)) {
    # in this case a concept record ID has been passed
    if ('safe' %in% zenodoRecord$hits$hits$metadata$communities[[1]]$id) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    # we have an unknown type
    return(FALSE)
  } 
}

updateIndex <- function (id, isConceptId = TRUE, dir = NULL) {
  #' Update the SAFE index file
  #' 
  #' The SAFE index file contains version and file-download information for all
  #' SAFE projects that have been requested by the user through the 
  #' \code{safedata} package. The intended use of the index file is to provide
  #' an offline cache of information associated with the varied records on the
  #' SAFE Zenodo database to streamline operation of \code{safedata} and 
  #' reduce redundancies. The index file is a dataframe stored (by default) in
  #' the \code{SAFE_data_dir}. Records are grouped by concept ID. Meta 
  #' information, including DOI, record ID, date created, ddataset embargo
  #' status, open date, and local file download status (TRUE/FALSE) are then
  #' saved for each record within the concept IDs requested. Every time a call
  #' is made to the Zenodo database for a SAFE dataset, \code{updateIndex} runs,
  #' either adding new version data or overwriting existing entries, depending
  #' on whether the concept ID has been previously requested.
  #' 
  #' \code{updateIndex} takes a Zenodo ID as its main input, which may be a
  #' concept ID (the default, \code{isConceptId = TRUE}), or the ID of a
  #' specific record entry (\code{isConceptId = FALSE}). Under the default
  #' behaviour, information pertaining to all versions of a particular concept
  #' are accessed through a call to \code{\link{zenodoVersionsApiLookup}} and
  #' stored within the index file. When a record ID is passed as input, it is
  #' assumed that it is because the dataset associated with this version has
  #' been downloaded and, after a directory check, the download status of this
  #' entry is modified. At the end of each call the modified index dataframe is
  #' saved in the local SAFE directory.
  #' 
  #' The SAFE index file is used downstream to identify and notify the user of
  #' the most recent available versions of a project, or in the event that the
  #' user is attempting to access a dataset that already exists within the
  #' \code{SAFE_data_dir}.
  #' 
  #' @param id the concept ID/record ID associated with the version of the SAFE
  #'   project being accessed
  #' @param isConceptId boolean indicating whether parameter \code{ID} is a 
  #'   concept ID or a record ID. This changes the operation of the function
  #' @param dir the directory into which the index.rds file should be stored,
  #'   which defaults to the \code{SAFE_data_dir}
  #' @seealso \code{\link{zenodoVersionsApiLookup}}, \code{\link{setSafeDir}},
  #'   \code{\link{buildVersionsDataframe}}
  #' @examples
  #'   # add version information for concept ID 3081058
  #'   setSafeDir("C:/Users/User/SAFE_data/")
  #'   updateIndex(3081058)
  #'   
  #'   # update download status of record 3081059 to TRUE
  #'   updateIndex(3081059, isConceptId = FALSE)
  #' @export
  
  # check directory - if not supplied and SAFE_data_dir not set then error
  if (is.null(dir)) {
    if (is.null(getOption('SAFE_data_dir'))) {
      stop(paste0('Directory not supplied and SAFE_data_dir not set! See ',
                  'setSafeDir() function'))
    } else {
      dir = getOption('SAFE_data_dir')
    }
  }
  
  # check that the index file exists (if so modify, otherwise create new)
  if (file.exists(file.path(dir, 'index.rds'))) {
    index <- readRDS(file.path(dir, 'index.rds'))
  } else {
    if (isConceptId) {
      index <- data.frame()
    } else {
      stop('No index.rds file found at the specified directory!')
    }
  }
  
  if (isConceptId) {
    # build the versions table associated with the concept ID
    allVersions <- zenodoVersionsApiLookup(id)
    versionsDf <- buildVersionsDataframe(allVersions)
    
    # add download info for each record in the concept
    versionsDf$downloaded <- as.logical(
      unlist(lapply(mapply(
        file.path, dir, buildFilePathFromVersions(allVersions)), 
        checkRecordDownloaded), use.names = FALSE))
    
    # order the versions by date (newest first)
    versionsDf <- versionsDf[order(versionsDf$created_on, decreasing = TRUE), ]
    
    # combine with the index meta-table and save the resulting table
    # two cases:
    #   the concept ID is already in the index (replacement), or
    #   concept ID is not in the index (add to the index)
    if (any(index$conceptId == id)) {
      inds <- which(index$conceptId == id)
      index[inds, ] = versionsDf
    } else {
      index <- rbind(index, versionsDf)
    }
  } else {
    # assume that a record ID has been passed and want to update the download
    # status (from not-downloaded to downloaded). Check in the index file...
    if (!any(index$recordId == id)) {
      stop(paste0('Record ID ', id, ' not found in index file'))
    } else {
      # get list of files in the directory
      files <- list.dirs(file.path(dir, index$conceptId[index$recordId == id], id))
      
      # if any files in the directory have '.xlsx' extension then set download
      # status to TRUE, otherwise set to FALSE
      if (any(grepl('.xlsx', unlist(files)))) {
        index$downloaded[index$recordId == id] = TRUE
      } else {
        index$downloaded[index$recordId == id] = FALSE
      }
    }
  }
  
  # save the modified index table
  saveRDS(index, file = file.path(dir, 'index.rds'))
}

buildVersionsDataframe <- function (allVersions) {
  #' Build a "versions" dataframe of a SAFE concept ID 
  #' 
  #' Takes the output from a call to the Zenodo 'versions' API and processes the
  #' returned \code{list} into a dataframe storing version details for all
  #' records associated with a given SAFE project (identified using the concept
  #' record ID number). Version fields include the DOI, record ID, creation
  #' date, access status ('open', 'embargoed', or 'closed'), and - in the case 
  #' of embargoed records - the open date (the date that the record will be
  #' publicly accessible) for all records associated with the concept ID.
  #' 
  #' @param allVersions the full versions list returned from a call to 
  #'   \code{\link{zenodoVersionsApiLookup}}
  #' @return a dataframe containing various information for each record version
  #'   associated with a particular SAFE concept ID
  #' @seealso \code{\link{zenodoVersionsApiLookup}}, \code{\link{updateIndex}}
  #' @export
  
  # check that the result of a valid versions API call has been passed
  if (length(allVersions$aggregations$access_right$buckets) == 0) {
    stop(paste0('Invalid API call'))
  }
  
  # create a dataframe with record ID, date, and access status
  open_date <- allVersions$hits$hits$metadata$embargo_date
  if(is.null(open_date)) {
	  open_date <- NA
  }
  
  versionsDf <- data.frame(
    conceptId    = allVersions$hits$hits$conceptrecid,
    doi          = allVersions$hits$hits$doi,
    recordId     = as.character(lapply(allVersions$hits$hits$doi, 
                                         getRecordIdFromDoi)),
    created_on   = as.Date(allVersions$hits$hits$created),
    access_right = allVersions$hits$hits$metadata$access_right,
    open_date    = open_date,
    stringsAsFactors = FALSE)
  
  # return the output
  return(versionsDf)
}

buildFilePathFromVersions <- function (versions) {
  #' Builds a list of file paths to SAFE datasets in the versions list
  #' 
  #' Using the output from a call to the Zenodo "versions" API
  #' (\code{\link{zenodoVersionsApiLookup}}), this function builds a set of
  #' pathways to local copies of downloaded datasets within a given SAFE concept
  #' ID. The structure of these pathways is governed by the \code{safedata}
  #' directory protocol, i.e. concept_ID > record_ID > file_name.
  #' 
  #' @param versions list of all record versions contained within a given
  #'   concept ID. The output from a call to \code{\link{zenodoVersionsApiLookup}}
  #' @return a list object containing (prospective) file paths to the data files
  #'   associated with each record within the concept ID, constructed according
  #'   to the \code{safedata} directory structure
  #' @seealso \code{\link{zenodoVersionsApiLookup}}
  #' @export
  
  if (class(versions) != 'list') {
    stop(paste0('buildFilePath expected list, got ', class(versions)))
  }
  
  numVers <- length(versions$hits$hits$conceptrecid)
  if (is.null(numVers)) {
    stop(paste0('Invalid input supplied, please check input list is output ',
                'from zenodoVersionsApiLookup'))
  }
  
  filePaths <- list()
  for (i in 1:numVers) {
    filePaths[[i]] <- file.path(versions$hits$hits$conceptrecid[i],
                                versions$hits$hits$id[i],
                                versions$hits$hits$files[[i]]$key)
  }
  
  return(filePaths)
}

checkRecordDownloaded <- function (path) {
  #' Check if the record (file) at the specified path has been downloaded
  #' 
  #' @param path the full path to the prospective file
  #' @return \code{TRUE}/\code{FALSE} value indicating whether file was found at
  #'   the specified path
  #' @export
  
  if (length(path) == 0) {
    return(FALSE)
  } else if (file.exists(path)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

getRecordIdFromDoi <- function (DOI) {
  #' Extracts record ID from Zenodo DOI link
  #' 
  #' @param DOI DOI link for a Zenodo-stored project
  #' @return The record ID component associated with the DOI, i.e. the numeric
  #'   components that appear after the final "/"
  #' @export
  
  if (!grepl('zenodo', DOI)) {
    stop('Please supply a valid Zenodo-sourced DOI')
  }
  return(substr(strsplit(DOI, 'zenodo')[[1]][2], start = 2, stop = 9999))
}

processSafe <- function (id, dir, updateIndex = TRUE, overwrite = FALSE) {
  #' Get record info for specified SAFE project ID from the Zenodo database
  #' 
  #' This function returns a \code{list} object from an API call to the Zenodo
  #' database using the supplied SAFE project ID number. The API call contains
  #' information required to download the requested SAFE dataset into the
  #' specified directory.
  #' 
  #' The function will automatically create a data folder at the directory
  #' \code{dir} and (optionally, \code{updateIndex} boolean) updates the SAFE
  #' index offline cache - a table that records version and file information for
  #' SAFE datasets that have been requested by the user. The index cache is
  #' automatically downloaded into the \code{dir} the first time a record from
  #' a particular concept ID is accessed.
  #' 
  #' \code{processSafe} additionally performs a series of checks on the record.
  #' Specifically, an error is raised in the event that the ID does not exist on
  #' Zenodo, or is not located in the SAFE community, or if the record is
  #' unavailable (i.e. is 'embargoed' or 'closed'). A warning message will be
  #' presented if a more recent version of the requested record is available for
  #' download.
  #' 
  #' @param id SAFE project record ID number. This could be a concept record
  #'   ID or a DOI identifier for a specific record. In the event that a concept
  #'   record ID is passed, the function will attempt to return the most recent
  #'   'open' dataset associated with the project
  #' @param dir directory into which data will be stored
  #' @param updateIndex \code{boolean}, should the SAFE index file be updated?
  #' @param overwrite Should existing copies of the data be overwritten.
  #' @return \code{list} object containing information from the Zenodo API call
  #' @seealso \code{\link{zenodoRecordApiLookup}}, \code{\link{updateIndex}}
  #' @export
  
  # get record info using standard API call
  record <- zenodoRecordApiLookup(id)
  
  if (!is.null(record$status)) {
    # check for either 404 (error) or 301 (redirect) message
    if (record$status == 404) {
      stop(paste0('Could not retrieve record (ID: ', id, ') - ', record$message))
    } else if (record$status == 301) {
      warning('Supplied ID (', id, ') is a concept record ID, attempting to ', 
              'return most recent record version', 
              call. = FALSE, immediate. = TRUE)
      isConceptRecId <- TRUE
      conceptRecId <- id
      allVersions <- zenodoVersionsApiLookup(id)
    }
  } else {
    # a valid record ID has been passed
    isConceptRecId <- FALSE
    conceptRecId <- record$conceptrecid
    message('Requested record (ID ', id, ') is associated with concept record',
            ' ID ', conceptRecId)
    allVersions <- zenodoVersionsApiLookup(record$conceptrecid)
  }
  
  # check that the project is part of the SAFE community
  if (!isSafeRecord(allVersions)) {
    stop(paste0('Concept ID ', conceptRecId, ' is not in the SAFE community'))
  }
  
  # check whether a SAFE directory exists for the concept ID (create one if not)
  if (!dir.exists(file.path(dir, conceptRecId))) {
    dir.create(file.path(dir, conceptRecId))
  }
  
  # update the SAFE index and select rows matching the concept record ID
  if (updateIndex) {
    message('Updating SAFE index for concept record ID ', conceptRecId, '... ',
            appendLF = FALSE)
    updateIndex(conceptRecId, dir = dir)
    message('complete!')
  } else {
    warning('updateIndex set to FALSE, offline SAFE index will not be updated!')
  }
  safeIndex <- readRDS(file.path(dir, 'index.rds'))
  vers <- safeIndex[which(safeIndex$conceptId == conceptRecId), ]
  
  # perform access checks on the requested record
  # records may be embargoed (raise an error)
  # records may be closed (raise an error)
  # records may not be the latest available version (raise a warning)
  # records may already be downloaded (rase an error unless overwrite = TRUE)
  latestOpen <- vers[which(vers$access_right == 'open')[1], ]
  printVers <- vers[, c(1,3:7)]
  printVers[' '] <- rep('', nrow(vers))
  
  if (isConceptRecId) {
    if (is.na(latestOpen$recordId)) {
      stop(paste0('No open records found for concept record ID ', conceptRecId, 
                  '. Current record status:\n', versionPrintout), call. = FALSE)
    } else {
      if (!overwrite & latestOpen$downloaded) {
        printVers[printVers$recordId == latestOpen$recordId, ' '] <- '  <-REQUESTED'
        versionPrintout <- printVersions(printVers)
        stop(paste0('Access cancelled: most recent version (record ', 
                    latestOpen$recordId, ') has already been downloaded! ', 
                    'Current record status:\n', versionPrintout), call. = FALSE)
      } else {
        message(paste0('Accessing record ID ', latestOpen$recordId, '...'))
        return(zenodoRecordApiLookup(latestOpen$recordId))
      }
    }
  } else {
    printVers[printVers$recordId == record$id, ' '] <- '  <-REQUESTED'
    versionPrintout <- printVersions(printVers)
    if (record$metadata$access_right %in% c('embargoed', 'closed')) {
      if (!is.na(latestOpen$recordId)) {
        # there are other records that are open
        stop(paste0('Requested record (ID ', record$id, ') is ', 
                    record$metadata$access_right, '. Please select an open ',
                    'record for this concept ID. Current record status:\n',
                    versionPrintout), call. = FALSE)
      } else {
        # there are no other open records
        stop(paste0('Requested record (ID ', record$id, ') is ', 
                    record$metadata$access_right, '. There are no other open ',
                    'records for this concept ID. Current record status:\n',
                    versionPrintout), call. = FALSE)
      }
    } else if (latestOpen$recordId != record$id) {
      # a more recent version is available
      if (!overwrite & vers$downloaded[vers$recordId == record$id]) {
        stop(paste0('\nAccess cancelled: record ', record$id, ' has already ',
                    'been downloaded. Newer versions are available. Current ',
                    'record status:\n', versionPrintout), 
             call. = FALSE)
      } else {
        warning(paste0('A more recent version of the requested record is ',
                       'available. Current record status:\n', versionPrintout), 
                call. = FALSE, immediate. = TRUE)
      }
    }
    # check whether we already have the record
    if (!overwrite & vers$downloaded[vers$recordId == record$id]) {
      stop(paste0('\nAccess cancelled: record ', record$id,
                  'has already been downloaded!'), call. = FALSE)
    } else {
      return(record)
    }
  }
}

printVersions <- function (printVers) {
  #' Returns print-friendly table displaying versions information for a given
  #' SAFE concept ID
  #' 
  #' @param printVers dataframe of versions information
  #' @return print-friendly table in the form of a string
  #' @seealso \code{\link{processSafe}}
  
  return(paste0(
    utils::capture.output(print(printVers, row.names = FALSE, right = FALSE)),
    collapse = '\n'))
}

downloadSafe <- function (zenodoRecord, dir = NULL) {
  #' Download SAFE project data file associated with the supplied record
  #' 
  #' Attempts to download the data file associated with the supplied Zenodo
  #' record, which should be a part of the SAFE community. The file will be
  #' downloaded into the specified directory (\code{dir}). \code{safedata}
  #' version 0.1 currently supports only .xlsx files. Future releases may be
  #' expanded to enable downloading of other file types.
  #' 
  #' @param zenodoRecord \code{list} object returned from Zenodo database API
  #'   call. This should be a call to the desired SAFE project
  #' @param dir directory into which data file should be downloaded
  #' @seealso \code{\link{zenodoRecordApiLookup}}, \code{\link{setSafeDir}}
  #' @export

  if (is.null(zenodoRecord$links$bucket)) {
    stop(paste0('No download link found for record, please check record ID'))
  } else {
    bucket <- jsonlite::fromJSON(zenodoRecord$links$bucket)
    fileName <- file.path(dir, basename(bucket$contents$links$self))
    if (tools::file_ext(fileName) != 'xlsx') {
      stop(paste0('Unable to access file type "', tools::file_ext(fileName),
                  '", only xlsx format is currently supported'))
    } else {
      message(paste0('\nDownloading file ', basename(fileName), '... '), 
              appendLF = FALSE)
      utils::download.file(bucket$contents$links$self, destfile = fileName, quiet = TRUE)
      message('complete!\n')
    }
  }
}

getSafe <- function (ids, dir = NULL, ...) {
  #' Get the SAFE files with the specified IDs
  #'
  #' This function processes and downloads the data files associated with the
  #' SAFE project IDs supplied. Each ID (which may be either a concept record ID
  #' or specific record ID) is validated as being from a SAFE community project,
  #' checked for versioning and access permissions, and finally downloaded into
  #' the relevant folder. Lastly, the ID downloaded statuses are updated in the
  #' SAFE index.rds file. 
  #' 
  #' @param ids an individual or array of project ID numbers to be downloaded
  #' @param dir SAFE data directory, which defaults to the \code{SAFE_data_dir} 
  #'   option. A new directory is created for each concept record ID. Data for
  #'   individual records are stored within the concept record ID directory
  #' @param ... optional argument to be passed to \code{\link{processSafe}}
  #' @seealso \code{\link{processSafe}}, \code{\link{downloadSafe}}
  #' @examples
  #'   getSafe(3081059, "C:/Users/User/Desktop/SAFE_data/")
  #'   getSafe(c(3081059, 2537074), "C:/Users/User/Desktop/SAFE_data/")
  #' @export
  
  # check whether directory has been supplied, use SAFE_data_dir if not
  if (is.null(dir)) {
    message('Active directory not specified, using SAFE_data_dir (', 
            appendLF = FALSE)
    if (!is.null(getOption('SAFE_data_dir'))) {
      dir <- getOption('SAFE_data_dir')
      message(paste0(dir, ')\n'))
    } else {
      message(paste0(getwd(), ')\n'))
      setSafeDir()
      dir <- getOption('SAFE_data_dir')
    }
  }
  
  # download files from Zenodo
  for (id in ids) {
    zenodoRecord <- processSafe(id, dir, ...)
    recordDir <- file.path(dir, zenodoRecord$conceptrecid, zenodoRecord$id)
    if (!file.exists(recordDir)) {
      dir.create(recordDir)
    }
    downloadSafe(zenodoRecord, recordDir)
    updateIndex(zenodoRecord$id, isConceptId = FALSE, dir = dir)
  }
}
