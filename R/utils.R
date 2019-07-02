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
		
		# add flags to show what information and files are held locally
		index$local_metadata <- FALSE
		index$local_file <- FALSE
		
		# format the datetime classes
		index$publication_date <- as.POSIXct(index$publication_date)
		index$dataset_embargo <- as.POSIXct(index$dataset_embargo)
		
		saveRDS(index, file.path(dir, 'safe_data_index.rds'))
	} else if(! index_found) {
		stop("Index not found, not a SAFE data directory.")
	} 
	
	# set the directory in options and cache the index 
	options(safedata.dir = dir)
	assign('index', readRDS(file.path(dir,'safe_data_index.rds')), safedata.env)
	
}

extract_record_id <- function(record_id){
	#' Extracts a dataset record id from user input
	#'
	#' Users could possible submit a range of values as a record id
	#' and this handler attempts to extract the record id number
	#' from the possible URLs.
	
	if(mode(record_id) == 'numeric'){
		if(record_id %% 1 == 0){
			return(record_id)
		} else {
			stop('Record id is not an integer')
		}
	} else if(mode(record_id) == 'character'){
		# Could be just a string representation of the integer or
		# could be a DOI (possibly with URL) or a Zenodo URL
		# - https://www.zenodo.org/record/3247631#.XRreJ9NKgWo
		# - https://doi.org/10.5281/zenodo.3247631
		# - 10.5281/zenodo.3247631
		# All of which end with the record id, except the ?tracking tag
		# on Zenodo
		match <- regexpr('^[0-9]+$|(?<=record/)[0-9]+|(?<=zenodo.)[0-9]+', record_id, perl=TRUE)
		if(match != -1){
			record_id <- regmatches(record_id, match)
			return(as.numeric(record_id))
		} else {
			stop('Unknown string record_id representation')
		}
	} else {
		stop('Unknown record_id value')		
	}
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

concept_summary <- function(record_id){
	
	#' Show a summary for a dataset concept.
	#'
	#' This function prints out summary information on a SAFE dataset 
	#' concept and the set of records associated with it. If the record_id
	#' is not itself a concept id, then the function looks up the relevant 
	#' concept. The version table indicates which versions are still under
	#' embargo ('---'), the most recent available version ('>>>') and outdated
	#' version ('+++').
	#'
	#' @param record_id A reference to a SAFE concept record
	#' @export

	index <- get_index()
	record <- extract_record_id(record_id)

	# convert record to concept id and trap unknowns
 	if(record_id %in% index$zenodo_record_id){
		record_id <- with(index, zenodo_concept_id[zenodo_record_id == record_id][1])
	} else if(! record_id %in% index$zenodo_concept_id){
		stop('Unknown record id')
	}
	
	# get the rows to report, sort by publication date and cut into record chunks
	rows <- subset(index, zenodo_concept_id == record_id)
	rows <- rows[order(rows$publication_date, decreasing=TRUE),]
	
	# Assuming only ever one xlsx file associated with a record - almost certainly true
	xl_files <- rows[grepl('.xlsx$', rows$filename),]
	
	# Use first row (most recent) to print general header.
	cat('\nConcept summary\n')
	cat(sprintf('Title: %s\n', xl_files$dataset_title[1]))
	cat(sprintf('Concept ID: %i\n\n', xl_files$zenodo_concept_id[1]))
	
	# Version availability
	unavailable <- xl_files$dataset_embargo >= Sys.time()
	n_unavailable <- sum(unavailable, na.rm=TRUE)
	cat(sprintf('Versions: %i available, %i embargoed\n', nrow(xl_files) - n_unavailable, n_unavailable))
	
	# Version summary
	version_available <- ifelse(unavailable, '---', '+++')
	version_available[which(! unavailable)[1]] <- '>>>'
		
	version_table <- data.frame(latest_available = version_available,
								record_id = xl_files$zenodo_record_id,
								published = format(xl_files$publication_date, '%Y-%m-%d'),
								embargo = ifelse(is.na(xl_files$dataset_embargo) | 
												 xl_files$dataset_embargo < Sys.time(),
												 '--', format(xl_files$dataset_embargo, '%Y-%m-%d')),
								local_data = xl_files$local_copy)
	
	print(version_table, row.names=FALSE)
	cat('\n')
}

record_summary <- function(record_id){
	
	#' Show a summary for a dataset record ID.
	#'
	#' This function prints out summary information on a SAFE dataset 
	#' record ID. 
	#'
	#' The function return an error if a concept ID is provided.
	#' 
	#' @param record_id A reference to a SAFE record ID.
	#' @export
	
	index <- get_index()
	record_id <- extract_record_id(record_id)

	# convert record to concept id and trap unknowns
 	if(record_id %in% index$zenodo_concept_id){
		stop('record_summary requires a record ID not a concept ID')
	} else if(! record_id %in% index$zenodo_record_id){
		stop('Unknown record id')
	}
	
	# grab the xlsx row from the index for this record
	row <- subset(index, zenodo_record_id == record_id & grepl('.xlsx$', filename))
	
	# Get the record metadata
	metadata <- get_record_metadata(row$zenodo_record_id)
	
	# Print out a summary
	cat('\nRecord summary\n')
	cat(sprintf('Title: %s\n', row$dataset_title))
	
	authors <- metadata[[c('metadata','authors')]]
	surnames <- sapply(strsplit(authors$name, ','), '[', 1)
	cat(sprintf('Authors: %s\n', paste(surnames, collapse=', ')))

	cat(sprintf('Publication date: %s\n', format(row$publication_date, '%Y-%m-%d')))
	cat(sprintf('Record ID: %i;\nConcept ID: %i\n', row$zenodo_record_id, row$zenodo_concept_id))

	status <- metadata[[c('metadata', 'access')]]
	if(status == 'embargo' & row$dataset_embargo < Sys.time()){
		status <- 'open'
	}
	cat(sprintf('Status: %s\n', status))
	
	ext_files <- metadata[[c('metadata', 'external_files')]]
	if(! is.null(ext_files)){
		cat(sprintf('External files: %s\n', paste(ext_files, collapse=' ,')))
	}
	
	# Taxa reporting
	taxa <- metadata[['taxa']]
	if(length(taxa) > 0){
		cat(sprintf('Taxa: %i taxa reported\n', nrow(taxa)))
	}

	# Locations reporting
	locs <- metadata[['locations']]
	if(length(locs) > 0){
		cat(sprintf('Locations: %i locations reported\n', nrow(locs)))
	}
	
	# Data worksheets
	dwksh <- metadata[[c('metadata', 'dataworksheets')]]
	nm_nch <- max(nchar(dwksh$name))
	cl_nch <- max(ceiling(log10(dwksh$max_col)), 4)
	rw_nch <- max(ceiling(log10(dwksh$n_data_row)), 4)

	cat('\nData worksheets:')
	cat(sprintf('%*s %*s %*s %s', nm_nch, 'name', cl_nch, 'ncol', 
				rw_nch, 'nrow', 'description'), sep='\n')
	
	cat(with(dwksh, sprintf('%*s %*i %*i %s', nm_nch, name, cl_nch, max_col, 
							rw_nch, n_data_row, description)), sep='\n')
	cat('\n')
}

data_worksheet_summary <- function(record_id, name){
	
	#' Show a summary for a data worksheet within a record.
	#'
	#' This function prints out summary information on a data worksheet
	#' contained in one the worksheets of SAFE dataset Excel file.
	#'
	#' The function return an error if a concept ID is provided.
	#' 
	#' @param record_id A reference to a SAFE record ID.
	#' @param name The name of the data worksheet
	#' @export
	
	index <- get_index()
	record_id <- extract_record_id(record_id)

	# convert record to concept id and trap unknowns
 	if(record_id %in% index$zenodo_concept_id){
		stop('record_summary requires a record ID not a concept ID')
	} else if(! record_id %in% index$zenodo_record_id){
		stop('Unknown record id')
	}
	
	# grab the xlsx row from the index for this record
	row <- subset(index, zenodo_record_id == record_id & grepl('.xlsx$', filename))
	
	# Get the record metadata
	metadata <- get_record_metadata(row$zenodo_record_id)$metadata
	
	# Find the worksheet
	dwksh <- metadata$dataworksheets
	idx <- which(dwksh$name == name)
	if(! length(idx)){
		stop('Data worksheet not found')
	}
	
	dwksh <- dwksh[idx,]
	
	# Print out a summary
	cat('\nData worksheet summary\n')
	cat(sprintf('Record ID: %i\n', metadata$zenodo_record_id))
	cat(sprintf('Worksheet name: %s\n', dwksh$name))
	cat(sprintf('Description: %s\n', dwksh$description))
	cat(sprintf('Number of data rows: %s\n', dwksh$n_data_row))

	if(metadata$access == 'embargo'){
		embargo_date <- as.POSIXct(metadata$embargo_date)
		if(embargo_date >= Sys.time()){
			cat(sprintf('Data embargoed until %s, only metadata available\n', 
				    	format(embargo_date, '%Y-%m-%d')))
		}
	}
	
	cat('\nFields:\n')
	fields <- dwksh['fields'][[1]][[1]]
	print(subset(fields, select=c(field_name, field_type, description)), row.names=FALSE)
	cat('\n')
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
