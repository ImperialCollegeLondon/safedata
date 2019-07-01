# Create an environment within the package namespace that is used to 
# keep a copy of the index, rather than needing to repeatedly read
# from file.

safedata.env <- new.env(parent = emptyenv())


simpleCap <- function (str) {
  #' Capitalize the first letter of each word in a string
  #' 
  #' This function takes a string, capitalizes the first letter of each word
  #' within it, removes all whitespace, and combines the result into a single
  #' word, returning the result.
  #' 
  #' @param str The string to be capitalized
  #' @return The original string pasted together with all whitespace removed and
  #'   the first letter of each word capitalized
  #' @examples
  #'   simpleCap('The quick brown fox jumps over the lazy dog')
  
  x <- strsplit(str, ' ')[[1]]
  return(
    paste0(toupper(substring(x, 1, 1)), substring(x, 2), sep = '', 
           collapse = ' '))
}

setSafeDir <- function(dir, init=FALSE) {
    #' Set the local SAFE data directory
    #'
    #' Validate and sets the local SAFE data directory in options('safedata.dir').
	#' The named directory must contain the index file ('safe_data_index.rds'),
	#' although this will be created if \code{init} is set to true.
	#'
	#' This function requires access to the internet to query the SAFE Project
	#' website API.
    #'
    #' @param dir A path to the directory to set as the SAFE data directory.
	#' @param init A boolean to indicate whether to initialise the directory
	#'     as a SAFE data directory.
	#' @export

	if (! dir.exists(dir)) {
        stop("Directory not found.")
    } 
	
	index_found <- file.exists(file.path(dir, 'safe_data_index.rds'))
	
	if(init & index_found) {
		warning("Index already exists, setting as SAFE data directory,.")
	} else if(init & ! index_found) {
		message('Downloading SAFE dataset index.')
		# Get the data frame of files as the index, set local = FALSE for all
		# and save as an RDS file in the data directory
		index <- jsonlite::fromJSON('https://www.safeproject.net/api/files')
		index <- index$entries
		index$local_copy <- FALSE
		saveRDS(index, file.path(dir, 'safe_data_index.rds'))
	} else if(! index_found) {
		stop("Index not found, not a SAFE data directory.")
	} 
	
	options(safedata.dir = dir)
	assign('index', readRDS(file.path(dir,'safe_data_index.rds')), safedata.env)
	
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

get_index <- function(){
	#' Get SAFE datasets index
	#'
	#' Internal handler to retrieve the local loaded copy of the index from the
	#' package cache environment
	#' @keywords internal
	
	local <- try(get('index', safedata.env), silent=TRUE)
	
	if(inherits(local, 'try-error')){
		stop('Failed to load cached index.')
	}
	
	return(local)
}


get_record_metadata <- function(record_id){
	#' Get SAFE dataset metadata
	#'
	#' Internal handler to load a local copy of the record metadata or retrieve
	#' it from the SAFE project website if there is no local copy. Much of this
	#' data is also available from Zenodo, but the SAFE API includes taxon and
	#' location metadata.
	#' @keywords internal
	
	# Check the record id is in the index and get the concept id
	dir <- get_data_dir()
	index <- get_index()
	if(! record_id %in% index$zenodo_record_id){
		stop('Unknown record id')
	} else {
		concept_id <- with(index, zenodo_concept_id[zenodo_record_id == record_id][1])
	}
	
	# Don't redownload if it is already local, and cache a copy if it does need
	# to be downloaded.
	local_path <- file.path(dir, concept_id, record_id, sprintf('%i.rds', record_id))
	if(file.exists(local_path)){
		record <- readRDS(local_path)
	} else {
		record <- jsonlite::fromJSON(sprintf('https://www.safeproject.net/api/record/%i', record_id))
		if(! dir.exists(dirname(local_path))){
			dir.create(dirname(local_path), recursive=TRUE)
		}
		saveRDS(record, local_path)
	}
	
	return(record)
}




update_safe_data_index <- function(){
	
	#' Updates the SAFE data index
	#' 
	#' This function compares the local index file ('safe_data_index.rds') 
	#' and attempts to update it from the SAFE Project website API. It 
	#' requires access to the internet.
	#' @export
	
	dir <- get_data_dir()
	local <- get_index()
	
	# Get the local current index from the website
	current <- jsonlite::fromJSON('https://www.safeproject.net/api/files')
	
	# Currently just using the set of zenodo record ids to test.
	# It is conceivable that entries could change after being created,
	# so maybe need a more complex reset at some point to look for 
	# datasets that have changed.
	if(setequal(local$zenodo_record_id, current$zenodo_record_id)) {
		message('Index up to date')
	} else if(length(setdiff(local$zenodo_record_id, current$zenodo_record_id)) > 0){
		message('Local index contains retracted or unknown records, contact data@safeproject.net')
	} else {
		message('Updating index')
		# merge in the information on local copies and set local copy 
		# for new rows to FALSE
		local <- subset(local, select=c(zenodo_record_id, local_copy))
		current <- merge(current, local, by='zenodo_record_id')
		current$local_copy[is.na(current$local_copy)] <- FALSE
		
		# Update the index cache and RDS file
		saveRDS(current, file.path(dir, 'safe_data_index.rds'))
		assign('index', current, safedata.env)
	}
}

readTransposedXlsx <- function (path, sheetName, ...) {
  #' Read a transposed .xlsx file into a dataframe
  #' 
  #' Provides capability to read .xlsx files that are transposed (i.e. 
  #' organised with headers in rows and data across columns) into a standard R
  #' dataframe. This function preserves data types.
  #' 
  #' @param path The path to the .xlsx file to be opened
  #' @param sheetName The name of the worksheet to be imported
  #' @param ... Optional arguments to be passed to \code{read_xlsx}
  #' @return The reformatted .xlsx with headers as columns and data in rows
  #' @seealso \code{\link[readxl]{read_xlsx}}
  
  df <- suppressMessages(
    readxl::read_xlsx(path, sheet = sheetName, col_names = FALSE, ...))
  dfT <- as.data.frame(t(df[-1]), stringsAsFactors = FALSE)
  names(dfT) <- t(df[,1])
  dfT <- as.data.frame(lapply(dfT, utils::type.convert))
  
  return(dfT)
}
