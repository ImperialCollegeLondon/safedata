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

get_index <- function(){
	
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


get_remote_index <- function(){
	
	#' Retrieve current dataset index
	#' 
	#' This function downloads the dataset index from the SAFE project
	#' files API (\url{https://www.safeproject.net/api/files}), formats
	#' datetime fields and adds local file paths.
	#'
	#' @keywords internal
	
	# Get the data frame of files as the index, set local = FALSE for all
	# and save as an RDS file in the data directory
	
	index <- jsonlite::fromJSON('https://www.safeproject.net/api/files')
	index <- index$entries
		
	# format the datetime classes
	index$publication_date <- as.POSIXct(index$publication_date)
	index$dataset_embargo <- as.POSIXct(index$dataset_embargo)
	
	# Add the path relative to the data directory
	index$path <- with(index, file.path(zenodo_concept_id, zenodo_record_id, filename))
	
	return(index)
	
}


set_index_availability <- function(index){
	
	#' Update dataset availability information
	#' 
	#' This function gets the index and sets which datasets are
	#' currently and which records are the most recent available 
	#' under a given concept.
	#'
	#' @param index An index data frame
	#' @return An updated index data frame with added fields showing
	#'   available and most recent available records.
	#' @keywords internal
			
	
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
		 
	return(index)
}
