#' Dataset index cache
#'
#' The \code{safedata.env} environment within the package namespace is 
#' used to cache a copy of the dataset index, gazetteer and location aliases
#' rather than needing to repeatedly read these from file. This environment
#' is accessed by internal functions and is not intended for the end user.
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
	#' @examples
	#'    safedir <- system.file('example_data_dir', package='safedata')
	#'    set_safe_dir(safedir)
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
						 
		metadata_json <- local_files[grepl('[0-9]+/[0-9]+/[0-9]+.json$', local_files)]	
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
