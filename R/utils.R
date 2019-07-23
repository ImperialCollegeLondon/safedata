#' Dataset index cache
#'
#' The \code{safedata.env} environment within the package namespace is 
#' used to cache a copy of the dataset index, rather than needing to 
#' repeatedly read from file. This index is used internally by functions
#' through the internal \code{get_index} function and is not intended
#' for the end user.
#'
#' @keywords internal
#' @aliases index

safedata.env <- new.env(parent = emptyenv())


set_safe_dir <- function(safedir, update=TRUE, create=FALSE, validate=TRUE){
	
    #' Set the local SAFE data directory
    #'
	#' This function sets the local directory used to store SAFE dataset
	#' files along with record and index metadata. The function can also
	#' initialise a new data directory, downloading the required index 
	#' files. By default, it will update indices if needed and will 
	#' validate the directory contents. Once set, the location of the 
	#' directory is stored in options('safedata.dir').
	#'
	#' The safedata package uses a data directory to store local copies of 
	#' dataset files along with index files. Files for a dataset record are
	#' stored in subdirectories using the zenodo concept id for the record 
	#' and then record id: \code{3342494/3342495}, for example. In addition 
	#' to data files from these records, these folders can also contain a
	#' JSON file containing record metadata (e.g. \code{3342495.json}): this
	#' is a structured version of the summary information shown on the 
	#' Zenodo page.
	#' 
	#' The root of the data directory also holds three index files:
	#' \describe{
	#'   \item{\code{index.json}}{, containing a full list of the
	#' 	    files and dataset records available in the SAFE data 
	#' 		repository;}
	#'   \item{\code{gazetteer.geojson}}{, containing the official list of 
	#' 	    known sampling locations and GIS data; and}
	#'   \item{\code{location_aliases.csv}}{, a list of alternative names 
	#' 	    permitted for some locations.}
	#' }
	#' 
	#' If \code{create=TRUE}, the function will try to create the named
	#' directory and populate it with the three index files. This requires
	#' an internet connection.
	#' 
	#' By default, the function also needs an internet connection to check
	#' for updates to the three index files. Updating can be turned off for 
	#' offline use.
	#' 
	#' The default behaviour is also to validate the directory structure. The
	#' function will warn when files other than those found in datasets are 
	#' present within the data structure and when any dataset files that are
	#' present have been modified. Although this can be turned off, it is 
	#' not recommended to modify or add files within a SAFE data directory.
	#' 
    #' @param safedir A path to the directory to set as the SAFE data 
	#'    directory (str).
	#' @param update Should the local dataset index be updated (logical)?
	#' @param create Should a new data directory be created at the provided
	#'    path (logical)?
	#' @param validate Should the directory structure be validated (logical)?
	#' @return NULL
	#' @export
	
	# path expand to make paths work in md5sum, which fails with ~/.
	safedir <- path.expand(safedir)
	
	index_path <- file.path(safedir,'index.json')
	gazetteer_path <- file.path(safedir,'gazetteer.geojson')
	location_aliases_path <- file.path(safedir,'location_aliases.csv')
	
	# Handle create first
	if(create) {
		
		# We don't want a existing directory that might have stuff in it
		if(dir.exists(safedir)) {
			stop('Directory already exists')
		} 
		
		# create the directory and set it as safe data directory
		dir.create(safedir)
		options(safedata.dir = safedir)
		
		# download the index file, then cache it
		download_index()
		cache_index()
		
		# download the gazetteer and location aliases
		download_gazetteer()
		download_location_aliases()
				
		verbose_message('Safe data directory created')
		return(invisible())
	} 
	
	# Now validate an existing directory
	
	if (! dir.exists(safedir)) {
        stop("Directory not found.")
    } 
	
	if (! file.exists(index_path)) {
		stop("Dataset index not found.")
	}
	
	if (! file.exists(gazetteer_path)){
		stop("Gazetteer not found.")
	}

	if (! file.exists(location_aliases_path)){
		stop("Location aliases not found.")
	}
	
	# Set the data directory and then load the index file into the cache and retrieve it for use
	options(safedata.dir = safedir)
	cache_index()
	index <- retrieve_index()
	
	# Look for updates
	if(update){
		
		verbose_message('Checking for updates')
		
		# Get the current index hashes from the SAFE Project API and 
		# then check each of the three index files
		index_hashes <- jsonlite::fromJSON('https://www.safeproject.net/api/index_hashes')

		# Check the index
		if(tools::md5sum(index_path) != index_hashes$index){
			verbose_message(' - Updating index')
			download_index()
			# reload the index into the cache and get it
			cache_index()
			index <- retrieve_index()
		} else {
			verbose_message(' - Index up to date')
		}

		# Check the gazetteer
		if(tools::md5sum(gazetteer_path) != index_hashes$gazetteer){
			verbose_message(' - Updating gazetteer')
			download_gazetteer()
		} else {
			verbose_message(' - Gazetteer up to date')
		}
		
		# Check the gazetteer
		if(tools::md5sum(location_aliases_path) != index_hashes$location_aliases){
			verbose_message(' - Updating location aliases')
			download_location_aliases()
		} else {
			verbose_message(' - Location aliases up to date')
		}
	}
	
	if(validate){
		
		verbose_message('Validating directory')
		
		# Run a check on directory structure
		local_files <- dir(safedir, recursive=TRUE)
		
		# Exclude the three index files and local metadata json files
		index_files <- c(basename(index_path), basename(gazetteer_path), 
						 basename(location_aliases_path))
						 
		metadata_json <- local_files[grepl('[0-9]+/[0-9]+/[0-9]+.rds$', local_files)]	
		local_files <- setdiff(local_files, c(index_files, metadata_json))
	
		local_unexpected <- setdiff(local_files, index$path)
		if(length(local_unexpected)){
			warning('SAFE data directory contains unexpected files: ', paste(local_unexpected, collapse=', '))
		}
	
		# Run a check on file modification
		local_expected <- subset(index, file.exists(file.path(safedir, index$path)))
		local_expected$local_md5 <- tools::md5sum(file.path(safedir, local_expected$path))
		
		local_altered <- with(local_expected, filename[local_md5 != checksum])
		
		if(length(local_altered)){
			warning('Local copies of dataset files have been modified', paste(local_unexpected, collapse=', '))
		}
	}
		
	return(invisible())
}


download_index <- function(){
	
	#' Download the current dataset index
	#' 
	#' This function downloads the dataset index from the SAFE project
	#' index API (\url{https://www.safeproject.net/api/index}) and saves
	#' it in the root SAFE data directory
	#'
	#' @return NULL
	#' @keywords internal
	
	safedir <- get_data_dir()
	path <- file.path(safedir,'index.json')
	api <- 'https://www.safeproject.net/api/index'
	result <- try(curl::curl_download(api, path), silent=TRUE)
			  
	if(inherits(result, 'try-error')){
		stop('Failed to download index')
	} 
	
	return(NULL)	

}


download_gazetteer <- function(){
	
	#' Downloads the current SAFE gazetteer
	#' 
	#' This function downloads the gazetteer from the SAFE project
	#' gazetteer API (\url{https://www.safeproject.net/api/gazetteer})
	#' and saves it in the root of the safe data directory.
	#'
	#' @return NULL
	#' @keywords internal
	
	safedir <- get_data_dir()
	path <- file.path(safedir,'gazetteer.geojson')
	api <- 'https://www.safeproject.net/api/gazetteer'
	result <- try(curl::curl_download(api, path), silent=TRUE)
			  
	if(inherits(result, 'try-error')){
		stop('Failed to download gazetteer')
	} 
	
	return(NULL)	
}


download_location_aliases <- function(){
	
	#' Downloads the current SAFE location aliases
	#' 
	#' This function downloads the location aliases from the SAFE project
	#' location aliases API (\url{https://www.safeproject.net/api/location_aliases})
	#' and saves it in the root of the safe data directory.
	#'
	#' @return NULL
	#' @keywords internal
	
	safedir <- get_data_dir()	
	path <- file.path(safedir,'location_aliases.csv')
	api <- 'https://www.safeproject.net/api/location_aliases'
	result <- try(curl::curl_download(api, path), silent=TRUE)
			  
	if(inherits(result, 'try-error')){
		stop('Failed to download location aliases')
	}
	
	return(NULL)	
}


cache_index <- function(){
	
	#' Load the record index into memory
	#' 
	#' This function loads the dataset record index from the JSON
	#' file in the SAFE data directory and sets which datasets are
	#' currently available and which records are the most recent 
	#' available under a given concept.
	#'
	#' @return NULL
	#' @keywords internal
			
	safedir <- get_data_dir()
	index_path <- file.path(safedir,'index.json')
	
	index <- jsonlite::fromJSON(index_path)
	index <- index$entries
		
	# format the datetime classes
	index$publication_date <- as.POSIXct(index$publication_date)
	index$dataset_embargo <- as.POSIXct(index$dataset_embargo)
	
	# Add the path relative to the data directory
	index$path <- with(index, file.path(zenodo_concept_id, zenodo_record_id, filename))
	
	# Identify availability and most recent available records
	index$available <- with(index, 
							ifelse(dataset_access == 'embargo' & dataset_embargo >= Sys.time(), FALSE, 
								   ifelse(dataset_access == 'restricted', FALSE, TRUE)))
	
	# Get the index rows by concept, reduce to the unique set of records (dropping 
	# the multiple files), drop unavailable records, sort by publication date and 
	# return the first zenodo_record_id.
	concepts <- split(subset(index, select=c(available, zenodo_record_id, publication_date)), 
					  f=index$zenodo_concept_id)

	mr_avail <- sapply(concepts, function(recs){
		recs <- unique(recs)
		recs <- subset(recs, available)
		recs <- recs[order(recs$publication_date, decreasing=TRUE),]
		
		if(nrow(recs)){
			return(recs$zenodo_record_id[1])
		} else {
			return(numeric(0))
		}
	})
	
	index$most_recent_available <- with(index, ifelse(zenodo_record_id %in% unlist(mr_avail), TRUE, FALSE))
	
	# save the index into the cache
	assign('index', index, safedata.env)	
	return(invisible())
}


retrieve_index <- function(){
	
	#' Get the cached SAFE dataset index
	#'
	#' Internal handler to retrieve the local loaded copy of the index from the
	#' package cache environment
	#'
	#' @keywords internal
	
	index <- try(get('index', safedata.env), silent=TRUE)
	
	if(inherits(index, 'try-error')){
		stop('Failed to load cached index.')
	}
	
	return(index)
}


get_data_dir <- function(){
	#' Get SAFE data directory
	#' 
	#' Internal handler to check the data directory is set and return it.
	#' @keywords internal
	
	if(is.null(options('safedata.dir'))){
		stop('SAFE data directory not set.')
	}
	
	return(options('safedata.dir')$safedata.dir)
}


verbose_message <- function(str, ...){
	
	#' Provide package messages that can be globally muted
	#'
	#' Prints a message if  \code{option('safedata.verbose')}  is set to TRUE.
	#' Note that individual expressions can be muted using \code{suppressMessages()}
	#' but this mutes them globally.
	#' @keywords internal
	
	if(options("safedata.verbose")[[1]]){
		message(str, ...)
	}
}


validate_record_ids <- function(record_set){
	
	#' Validates dataset record ids from user input
	#'
	#' This takes a vector of user supplied record identifiers and validates them 
	#' against the index. Typically the identifiers are provided as integers, but
	#' the function will also handle Zenodo URLs and DOIs.
	#' 
	#' The function returns a data frame with class \code{safe_record_set}, containing
	#' the columns \code{concept}, \code{record}, \code{available} and, finally,
	#' \code{mra} containing the most recent available record (if any). The function can
	#' be run on an existing \code{safe_record_set} to update this information.
	#' 
	#' This function is largely used internally to validate user inputs and to provide
	#' a common output for the search functions but is exported to allow users to 
	#' check record ids and display summary information using the print method.
	#'
	#' @param record_set A vector of values containing Zenodo concept or record ids.
	#' @param x An object of class 'safe_record_set'
	#' @param ... Further arguments to print methods, unused.
	#' @return An object of class 'safe_record_set': a dataframe with columns
	#'   \code{concept} and \code{record} indicating the matching records. Note
	#'   that \code{record} will be NA when the value is a concept id. Inputs
	#'   that do not match a record or concept ids are returned as an attribute of
	#'   the record set.
	#' @examples
	#'   validate_record_ids(c(3247631, 3266827, 3266821, -1000))
	#'   validate_record_ids(c('https://doi.org/10.5281/zenodo.3247631', 
	#'						   '10.5281/zenodo.3266827', 
	#'						   'https://zenodo.org/record/3266821',
	#'						   'not_this_one/3266821'))
	#' @export
	
	index <- retrieve_index()
	
	# Only run validation if the input isn't already a record set
	if(! inherits(record_set, 'safe_record_set')){
	
		# Otherwise validate
		if(! (is.vector(record_set) && mode(record_set) %in% c('character', 'numeric'))){
			stop('record_set must be a character or numeric vector')
		}
	
		# store original versions
		user <- record_set
		
		if(mode(record_set) == 'numeric'){
		
			# If numbers, look for positive integers
			not_int <- record_set %% 1 != 0
			not_pos <- record_set <= 0
			valid <- (! not_int) & (! not_pos)

			if(any(! valid)){
				warning('Some record ids are not positive integers')
			}
				
		} else if(mode(record_set) == 'character'){
		
			# If string look for one of the possible string representations
			# of the record: a straight string of the integer or
			# could be a DOI (possibly with URL) or a Zenodo URL
			# - https://www.zenodo.org/record/3247631#.XRreJ9NKgWo
			# - https://doi.org/10.5281/zenodo.3247631
			# - 10.5281/zenodo.3247631

			match <- regexpr('^[0-9]+$|(?<=record/)[0-9]+|(?<=zenodo.)[0-9]+', record_set, perl=TRUE)
			valid <- match != -1
		
			if(any(! valid)){
				warning('Some record ids do not match known id formats')
			}
		
			out <- rep(NA,length(record_set))
			out[match != -1] <- regmatches(record_set, match)
			record_set <- as.numeric(out)
			valid <- match != -1
		}
			
		# Do they appear in the index as concept ids or record ids
		known <- record_set %in% c(index$zenodo_record_id, index$zenodo_concept_id)
	
		if(! all(known)){
			warning('Some values are not known concept or record ids')
		}
			
		record_set <- data.frame(concept = ifelse(record_set %in% index$zenodo_concept_id, record_set, NA),
								 record = ifelse(record_set %in% index$zenodo_record_id, record_set, NA))
	
		record_set$concept <- ifelse(is.na(record_set$concept), 
									 index$zenodo_concept_id[match(record_set$record, index$zenodo_record_id)],
									 record_set$concept)
		
	 	class(record_set) <- c('safe_record_set', 'data.frame')

		# remove duplicates
		record_set <- unique(record_set)
		
		# move bad records to attributes
		record_set <- subset(record_set, valid & known)
		mismatches <- unique(user[! known | ! valid])
		if(length(mismatches)){
			attr(record_set, 'mismatches') <- mismatches
		}
	}
	
	# Now add information on whether individual records are available and then, for the concept,
	# the most recent record (which might not be available) and the most recent available if there 
	# is one
    record_set$available <- index$available[match(record_set$record, index$zenodo_record_id)]

	most_recent <- subset(index, most_recent, select=c(zenodo_concept_id, zenodo_record_id))
	record_set$most_recent <- most_recent$zenodo_record_id[match(record_set$concept, most_recent$zenodo_concept_id)]
	
	mra <- subset(index, most_recent_available, select=c(zenodo_concept_id, zenodo_record_id))
	record_set$mra <- mra$zenodo_record_id[match(record_set$concept, mra$zenodo_concept_id)]

	# Sort by concept id (increasing from earliest) and then by record id (decreasing from 
	# most recent) and keep NAs at the top, so concept ids come first.
	record_set <- record_set[order(record_set$concept, record_set$record, 
									decreasing=c(FALSE, TRUE), method='radix', na.last=FALSE), ]
	
	return(record_set)	
}


print.safe_record_set <- function(x, ...){
	
	#' @describeIn validate_record_ids Print a brief summary of 'safe_record_set' objects.
	#' @export
	
	msg <- paste0('Set includes %i concept ids and %i record ids: \n',
				  ' - %i open and most recent (*)\n',
				  ' - %i open and outdated (o)\n',
				  ' - %i under embargo or restricted (x)\n\n')
	
	# record availability flags and counts 
	x$available <- with(x, ifelse(is.na(record), '', ifelse(! available, 'x', ifelse(record == mra, '*','o'))))
	n_status <- c('*'=0, 'x'=0, 'o'=0)
	counts <- table(x$available)
	n_status[names(counts)] <- counts
	n_records <- sum(!is.na(x$record))
		
	cat(sprintf(msg, nrow(x) - n_records, n_records, 
					 n_status['*'], n_status['o'], n_status['x']))
	
	# This relies on the sort order set in validate_record_ids
	x$concept <- ifelse(duplicated(x$concept), '-------', x$concept)
	x$record <- ifelse(is.na(x$record), '-------', x$record)
	
	class(x) <- 'data.frame'
	print(subset(x, select=c(concept, record, available)))

	
	if(! is.null(attr(x, 'mismatches'))){
		cat('\nUnmatched record ids:', paste(attr(x, 'mismatches'), sep=', '), '\n')
	}
	
	return(invisible())
}


fetch_record_metadata <- function(record_set){
	
	#' Get SAFE dataset metadata
	#'
	#' Internal handler to ensure there are local copies of record metadata,
	#' fetching it from the SAFE project website if needed. This is the same
	#' data used to populate the Zenodo description but is machine readable
	#' and contains additional taxon and location indexing.
	#'
	#' @param record_set An object of class \code{safe_record_set}.
	#' @return NULL
	#' @examples
	#'   recs <- c('https://doi.org/10.5281/zenodo.3247631', '10.5281/zenodo.3266827', 
	#'	           'https://zenodo.org/record/3266821', 'not_this_one/3266821')
	#'   recs <- validate_record_ids(recs)
	#'   fetch_record_metadata(recs)
	#' @keywords internal
	
	# Check the input class
	if(! inherits(record_set, 'safe_record_set')){
		stop('Expects a safe_record_set object.')
	}
	
	# Look for zenodo_record_id files only
	record_set <- unique(subset(record_set, ! is.na(record)))

	if(nrow(record_set)){
		
		safedir <- get_data_dir()
	
		# Find missing RDS files
		record_set$local_path <- with(record_set, file.path(safedir, concept, record, sprintf('%i.json', record)))
		record_set$to_download <- ! file.exists(record_set$local_path)
		
		record_set <- subset(record_set, to_download)
		
		if(nrow(record_set)){
			verbose_message('Downloading ', nrow(record_set), ' record metadata files\n')
		}
		
		for(idx in seq_along(record_set$record)){
			to_get <- record_set[idx,]
			
			if(! dir.exists(dirname(to_get$local_path))){
				dir.create(dirname(to_get$local_path), recursive=TRUE)
			}
			
			api <- sprintf('https://www.safeproject.net/api/record/%i', to_get$record)
			curl::curl_download(api, to_get$local_path)
		}
	}
	
	return(invisible())
}


load_record_metadata <- function(record_set){
	
	#' Loads the metadata for a record
	#'
	#' @param record_set An object of class \code{safe_record_set} containing a single
	#'   row with complete concept and record data.
	#' @keywords internal

	if((! inherits(record_set, 'safe_record_set')) && 
	   (nrow(record_set) == 1) && 
	   (! any(is.na(record_set)))
	  ){
		stop('Expects a single row safe_record_set object with complete concept and record id.')
	}

	# Ensure it is locally available 
	fetch_record_metadata(record_set)
	
	# load it and return it
	safedir <- get_data_dir()
	metadata_file <- with(record_set, file.path(safedir, concept, record, sprintf('%i.json', record)))
	return(jsonlite::fromJSON(metadata_file))
}


