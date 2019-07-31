load_safe_worksheet <- function(record_set,  worksheet, skip, n_max){
	
	#' Internal data loading function
	#'
	#' This is an internal function shared between load_safe_data (data worksheets)
	#' get_taxa (Taxa sheets) and get_locations (Locations sheets). 
	#'
	#' @param index_row A row selected from the index data frame, identifying the
	#'    workbook path.
	#' @param worksheet The name of the worksheet to load (str).
	#' @param skip The number of initial rows to skip (integer)
	#' @param n_max The number of rows to load (integer)
	#' @return A data frame of the worksheet, 
	#' @keywords internal
	
	# Look for a local copy of the file. If it doesn't exist, download it if possible
	
	index <- load_index()
	index_row <- subset(index, zenodo_record_id == record_set$record)
	
	local_path <- file.path(get_data_dir(), record_set$concept, record_set$record, index_row$filename)
	local_copy <- file.exists(local_path)
	
	if(! local_copy){
		verbose_message('Downloading datafile: ', index_row$filename)
		downloaded <- download_safe_files(index_row$zenodo_record_id)
		if(! length(downloaded)){
			stop('Data file unavailable')
		}
	}
	
	# Validate the local copy
	local_md5 <- tools::md5sum(local_path)
	if(local_md5 != index_row$checksum){
		stop('Local file has been modified - do not edit files within the SAFE data directory')
	}

	# Now load the data - using readxl, openxlsx is also possible but seems
	# to be orphaned and has some date time handling issues
	data <- readxl::read_xlsx(local_path, worksheet, skip = skip, n_max = n_max, na='NA')
						  
	# Discard the tibble class and first column of row numbers
	class(data) <- 'data.frame'
	data <- data[,-1]

	return(data)
}


load_safe_data <- function(record_id, worksheet){
	
	#' Loads data from a SAFE dataset.
	#'
	#' This function returns a data frame containing the data from a data 
	#' worksheet in a SAFE dataset. Note that SAFE dataset .xlsx files include
	#' the other (non-data) worksheets Summary, Taxa, Locations that contain 
	#' metadata: see  \code{\link{get_taxa}}, \code{\link{get_locations}}, 
	#' \code{\link{add_taxa}} and \code{\link{add_locations}} for accessing 
	#' and using this metadata.
	#' 
	#' In particular, the large amount of data worksheet summary metadata is
	#' not attached as attributes to the data frame returned by this function.
	#' This is largely to avoid excessive output to the console during normal use 
	#' of the data frame: an extended description of a worksheet can be displayed
	#' using \code{\link{show_worksheet}}.
	#'
	#' Currently, this function only loads data from SAFE formatted \code{.xlsx}
	#' files - data stored in external files is not yet handled.
	#'
	#' @param record_id A SAFE dataset record id 
	#' @param worksheet The name of the worksheet to load
	#' @param object A \code{safedata} object.
	#' @param \dots Further arguments to \code{str} methods.
	#' @return A data frame with the additional \code{safedata} class and
	#'    additional attribute data containing metadata for the data.
	#' @examples
	#'    safedir <- system.file('example_data_dir', package='safedata')
	#'    set_safe_dir(safedir)
	#'    ant_abund <- load_safe_data(1400562, 'Ant-Psel')
	#'    str(ant_abund)
	#'    # See also the show_worksheet function for further worksheet metadata
	#'    show_worksheet(ant_abund)
	#' @export
	
	# TODO - provide a path argument and then mechanisms to support a standalone file download?
	
	# validate the record id
	record_set <- validate_record_ids(record_id)
	
	# Logic of what to load.
	# a) If a record is provided, return that unless it is unavailable, in 
	#    which case suggest an alternative.
	# b) If a concept is provided, load MRA if there is one.
	
	
	if(nrow(record_set) != 1){
		stop("Requires a single valid record or concept id")
	} else if(is.na(record_set$record)){
		# concept provided
		if(is.na(record_set$mra)){
			stop("Concept ID provided: all records under embargo or restricted")
		} else {
			verbose_message("Concept ID provided: loading most recent available record")
			record_set <- validate_record_ids(record_set$mra)
		}
	} else {
		# record provided
		if(! record_set$available){
			if(is.na(record_set$mra)){
				stop("Record ID provided: this and all other versions of this dataset concept are under embargo or restricted")
			} else {
				stop("Record ID provided: version is under embargo or restricted. Most recent available is ", record_set$mra)
			}
		} else {
			if(! (record_set$record ==  record_set$mra)){
				verbose_message("Outdated record: the most recent available version is ", record_set$mra)
			}
		}
	}
	
	# Now get the metadata and find the target worksheet
	metadata <- load_record_metadata(record_set)
	if(! worksheet %in% metadata$dataworksheets$name){
		stop('Data worksheet name not one of: ', paste(metadata$dataworksheets$name, collapse=', '))
	}

	# If successful get the worksheet metadata and load the data
	dwksh <- metadata$dataworksheets[metadata$dataworksheets$name == worksheet, ]
	data <- load_safe_worksheet(record_set, worksheet, skip=dwksh$field_name_row - 1, n_max = dwksh$n_data_row)
	
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
	
	# Design notes on methods: we want safedata to behave as much as possible
	# like a data frame. The attributes are used to record provenance and the
	# safedata class is primarily used to allow loaded data frames to be passed
	# to the show_* metadata functions. With S3 generics, the set of classes is
	# used in order, so we only need to provide safedata S3 methods where we want
	# to modify the default dataframe methods - this is only str, where hiding the
	# attributes and displaying that information at top is aesthetically nicer.
	
	class(data) <- c('safedata', 'data.frame')
	dwksh <- as.list(dwksh)
	dwksh$safe_record_set <- record_set
	attr(data, 'metadata') <- dwksh
	return(data)
}


str.safedata <- function(object, ...){
	
	#' @describeIn load_safe_data Display structure of a safedata data frame
	#' @export
	
	object_attr <- attr(object, 'metadata')
	with(object_attr, cat(sprintf('SAFE dataset\nConcept: %i; Record %i; Worksheet: %s\n', 
								  safe_record_set$concept, safe_record_set$record, name)))
	
	# reduce the safedata object to a simple data frame and pass back to str(x, ...)
	attr(object, 'metadata') <- NULL
	class(object) <- 'data.frame'
	invisible(str(object, ...))

}

# TODO - sf safedata objects - fix print method to handle data frame and sf object and
#       adapt str to use NextMethod for the same reason.


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
	#' @param record_ids A vector of SAFE dataset record ids or a 
	#'    \code{\link{safe_record_set}} object.
	#' @param confirm Requires the user to confirm before download (logical)	
	#' @param xlsx_only Should all files be downloaded or just the core .xslx file (logical)
	#' @param download_metadata Should the metadata record for the file be downloaded (logical)
	#' @param refresh Should the function check if local copies have been modified and 
	#'   download fresh copies. This is useful if the local copies have unintentionally 
	#'   been modified but note the warning above.
	#' @param token An access token for restricted datasets. Not currently implemented.
	#' @return Invisibly, a vector of paths for successfully downloaded files.
	#' @examples
	#'    \donttest{
	#'    recs <- validate_record_ids(c(3247631, 3266827, 3266821))
	#'    download_safe_files(recs)
	#'    }
	#' @export
	
	# validate the record ids
	record_set <- validate_record_ids(record_ids)
	
	records_to_get <- record_set$record[! is.na(record_set$record)]
	
	if(! length(records_to_get)){
		verbose_message('No valid record ids provided')
		return(invisible())
	} 
	
	# Get the target files
	index <- load_index()
	safedir <- get_data_dir()
	
	# Get the set of files
	if(xlsx_only){
		targets <- subset(index, zenodo_record_id %in% records_to_get & grepl('.xlsx$', filename))
	} else {
		targets <- subset(index, zenodo_record_id %in% records_to_get)
	}
		
	# See what is stored locally
	targets$local_path <- file.path(safedir, targets$path)
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
	downloaded <- character()
	
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
							downloaded <- c(downloaded, this_file$local_path)
						}
					}
				}
			}
		}
	}
	
	return(invisible(downloaded))
}

