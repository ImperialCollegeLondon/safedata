#' Dataset index cache
#'
#' The \code{safedata.env} environment within the package namespace is 
#' used to store a copy of the dataset index, rather than needing to 
#' repeatedly read from file.
#' @keywords internal

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
		
	# identify availability and most recent available records
	index$available <- with(index, 
							ifelse(dataset_access == 'embargo' & dataset_embargo >= Sys.time(), FALSE, 
								   ifelse(dataset_access == 'restricted', FALSE, TRUE)))
	
	# get the index rows by concept, reduce to the unique set of records (dropping 
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

set_safe_dir <- function(safedir, update=TRUE, create=FALSE, validate=TRUE){
	
    #' Set the local SAFE data directory
    #'
    #' The safedata package maintains a local copy of data files, a dataset 
	#' index ('safe_data_index.rds') and dataset metadata in this directory.
	#' The location of the directory is stored in options('safedata.dir') and
	#' this function is used to point the package to an existing directory, 
	#' create a new data directory if requested and to maintain the dataset index.
	#' 
	#' By default (and by necessity if creating a new data directory), the 
	#' function requires the internet to update the dataset index, but updating
	#' can be turned off for offline use.
    #'
	#' The default behaviour is also to validate the directory structure. The
	#' function will warn when files other than those found in datasetes are 
	#' present within the data structure and when any dataset files that are
	#' present have been modified. Although this can be turned off, it is 
	#' not recommended to modify or add files within a SAFE data directory.
	#' 
    #' @param safedir A path to the directory to set as the SAFE data directory (str).
	#' @param update Should the local dataset index be updated (logical)?
	#' @param create Should a new data directory be created at the provided
	#'   path (logical)?
	#' @param validate Should the directory structure be validated (logical)?
	#' @return NULL
	#' @export

	
	index_path <- file.path(safedir,'safe_data_index.rds')
	
	# Handle create first
	if(create) {
		if(dir.exists(safedir)) {
			# We don't want a existing directory that might have stuff in it
			stop('Directory already exists')
		} 
		
		# create the directory
		dir.create(safedir)
		
		# create the new index file, stash in cache, set the options and end
		index <- get_remote_index()
		saveRDS(index, index_path)
		index <- set_index_availability(index)
		assign('index', index, safedata.env)
		options(safedata.dir = safedir)
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
	
	# At this point we have an existing index file, so update unless told not to
	index <- readRDS(index_path)
	
	if(update){
		verbose_message('Updating index...')
		remote <- get_remote_index()
		
		# Currently just using the set of zenodo record ids to test.
		# It is conceivable that entries could change after being created,
		# so maybe need a more complex reset at some point to look for 
		# datasets that have changed.
		if(setequal(index$zenodo_record_id, remote$zenodo_record_id)) {
			verbose_message('Index up to date')
		} else if(length(setdiff(index$zenodo_record_id, remote$zenodo_record_id)) > 0){
			verbose_message('Local index contains retracted or unknown records, contact data@safeproject.net')
		} else {		
			# Update the index cache and RDS file
			saveRDS(remote, file.path(safedir, 'safe_data_index.rds'))
			index <- remote
			verbose_message('Index updated')
		}
	}
	
	if(validate){
		verbose_message('Validating directory')		
		# Run a check on directory structure
		local_files <- dir(safedir, recursive=TRUE)
		local_files <- local_files[! grepl('.rds$', local_files)]	
	
		local_unexpected <- setdiff(local_files, index$path)
		if(length(local_unexpected)){
			warning('SAFE data directory contains unexpected files: ', paste(local_unexpected, collapse=', '))
		}
	
		# Run a check on file modification
		local_expected <- subset(index, file.exists(file.path(safedir, index$path)))
		local_expected$local_md5 <- tools::md5sum(path.expand(file.path(safedir, local_expected$path)))
		
		local_altered <- with(local_expected, filename[local_md5 != checksum])
		
		if(length(local_altered)){
			warning('Local copies of dataset files have been modified', paste(local_unexpected, collapse=', '))
		}
	}
	
	# set the directory in options and cache the index 
	options(safedata.dir = safedir)
	index <- set_index_availability(index)
	assign('index', index, safedata.env)
	
	return(invisible())
}


validate_record_ids <- function(record_ids){
	
	#' Validates dataset record ids from user input
	#'
	#' This takes a vector of user supplied record identifiers and validates them 
	#' against the index. It looks up the concept id for individual version 
	#' record ids. Typically the identifiers are integers, but the function
	#' will also handle Zenodo URLs and DOIs. 
	#' 
	#' The function is largely intended for internal use but can be used to 
	#' validate records before giving them to a function - the function returns
	#' an data frame with class \code{safe_record_set} that shows the values
	#' have been validated.
	#'
	#' @param record_ids A vector of record_id values
	#' @return An object of class 'safe_record_set': a dataframe with columns
	#'   \code{concept} and \code{record} indicating the matching records. Note
	#'   that \code{record} will be NA when thevalue is a concept id and both fields
	#'   will be NA if no match is found. The data frame rownames are used to 
	#'   record the original values provided.
	#' @examples
	#'   validate_record_ids(c(3247631, 3266827, 3266821, -1000))
	#'   validate_record_ids(c('https://doi.org/10.5281/zenodo.3247631', 
	#'						   '10.5281/zenodo.3266827', 
	#'						   'https://zenodo.org/record/3266821',
	#'						   'not_this_one/3266821'))
	#' @export
	
	# Don't revalidate
	if(inherits(record_ids, 'safe_record_set')){
		return(record_ids)
	}
	
	# Otherwise validate
	id_mode <- mode(record_ids)
	
	if(! (is.vector(record_ids) && id_mode %in% c('character', 'numeric'))){
		stop('record_ids must be a character or numeric vector')
	}
	
	# store original versions
	user <- record_ids
		
	if(mode(record_ids) == 'numeric'){
		
		# If numbers, look for positive integers
		not_int <- record_ids %% 1 != 0
		not_pos <- record_ids <= 0
		valid <- (! not_int) & (! not_pos)

		if(any(! valid)){
			warning('Some record ids are not positive integers')
		}
				
	} else if(mode(record_ids) == 'character'){
		
		# If string look for one of the possible string representations
		# of the record: a straight string of the integer or
		# could be a DOI (possibly with URL) or a Zenodo URL
		# - https://www.zenodo.org/record/3247631#.XRreJ9NKgWo
		# - https://doi.org/10.5281/zenodo.3247631
		# - 10.5281/zenodo.3247631

		match <- regexpr('^[0-9]+$|(?<=record/)[0-9]+|(?<=zenodo.)[0-9]+', record_ids, perl=TRUE)
		valid <- match != -1
		
		if(any(! valid)){
			warning('Some record ids do not match known id formats')
		}
		
		out <- rep(NA,length(record_ids))
		out[match != -1] <- regmatches(record_ids, match)
		record_ids <- as.numeric(out)
		valid <- match != -1
	}
	
	index <- get_index()
		
	# Do they appear in the index		
	known <- record_ids[valid] %in% c(index$zenodo_record_id, index$zenodo_concept_id)
	
	if(! all(known)){
		warning('Some values are not known concept or record ids')
	}
			
	record_ids <- data.frame(concept=ifelse(record_ids %in% index$zenodo_concept_id, record_ids, NA),
							 record= ifelse(record_ids %in% index$zenodo_record_id, record_ids, NA))
	
	record_ids$concept <- ifelse(is.na(record_ids$concept), 
								 index$zenodo_concept_id[match(record_ids$record, index$zenodo_record_id)],
								 record_ids$concept)
								 
	class(record_ids) <- c('data.frame', 'safe_record_set')
	rownames(record_ids) <- user
	return(record_ids)	
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
		record_set$local_path <- with(record_set, file.path(safedir, concept, record, sprintf('%i.rds', record)))
		record_set$to_download <- ! file.exists(record_set$local_path)
		
		record_set <- subset(record_set, to_download)
		
		for(idx in seq_along(record_set$record)){
			to_get <- record_set[idx,]
			
			record <- jsonlite::fromJSON(sprintf('https://www.safeproject.net/api/record/%i', to_get$record))
			if(! dir.exists(dirname(to_get$local_path))){
				dir.create(dirname(to_get$local_path), recursive=TRUE)
			}
			saveRDS(record, to_get$local_path)
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
	rds_file <- with(record_set, file.path(safedir, concept, record, sprintf('%i.rds', record)))
	return(readRDS(rds_file))
}


show_concepts <- function(record_ids){
	
	#' Show summary information on dataset concepts.
	#'
	#' This function takes a set of record ids and prints out summary information
	#' on the dataset concepts and the set of records associated with each one. 
	#'
	#' If the record_i
	#' is not itself a concept id, then the function looks up the relevant 
	#' concept. The version table indicates which versions are available ('<<<' 
	#' for the most recent available version and 'o' for older available versions),
	#' and which are unavailable due to embargo or retriction ('x').
	#'
	#' @param record_ids References to a SAFE dataset records or concepts or an
	#'   object of class \code{safe_record_set}.
	#' @return NULL
	#' @export
	
	# validate the record ids
	record_set <- validate_record_ids(record_ids)
	
	# get the rows to report, sort by publication date and cut into record chunks
	index <- get_index()
	
	rows <- subset(index, zenodo_concept_id %in% record_set$concept,
				   select=c(zenodo_concept_id, zenodo_record_id, dataset_title,
					   		publication_date, available, most_recent_available, 
							dataset_embargo))
	
	rows <- unique(rows)	
	
	concepts <- split(rows, f=rows$zenodo_concept_id)
	
	print_fun <- function(concept){
		
		concept <- concept[order(concept$publication_date, decreasing=TRUE),]
		
		# compile a list of lines
		text <- sprintf('\nConcept ID: %i', concept$zenodo_concept_id[1])
		text <- c(text, sprintf('Title: %s', concept$dataset_title[1]))
		
		# Version availability
		n_avail <- sum(concept$available)
		n_unavail <- nrow(concept) - n_avail 
		
		text <- c(text, sprintf('Versions: %i available, %i embargoed or restricted\n', n_avail, n_unavail))
	
		# Version summary
		version_available <- ifelse(concept$available, 'o', 'x')
		version_available[which(concept$most_recent_available)[1]] <- '<<<'
		
		version_table <- data.frame(record_id = concept$zenodo_record_id,
									published = format(concept$publication_date, '%Y-%m-%d'),
									embargo = ifelse(is.na(concept$dataset_embargo) | 
													 concept$dataset_embargo < Sys.time(),
													 '--', format(concept$dataset_embargo, '%Y-%m-%d')),
									available = version_available)
	
		text <- c(text, utils::capture.output(print(version_table, row.names=FALSE)), '\n')
		return(text)
	}
	
	text <- lapply(concepts, print_fun)
	text <- sapply(text, paste0, collapse='\n')
	
	cat(paste0(text, collapse='-------------\n'))

	return(invisible())
}

show_record <- function(record_id){
	
	#' Show a summary for a dataset record ID.
	#'
	#' This function prints out summary information on a SAFE dataset 
	#' record ID. 
	#'
	#' The function return an error if a concept ID is provided.
	#' 
	#' @param record_id A reference to a SAFE record ID.
	#' @return NULL
	#' @export
	
	record_set <- validate_record_ids(record_id)
	
	if(nrow(record_set) != 1){
		stop('show_record requires a single record id')
	}
	
	if(all(is.na(record_set))){
		stop('Unknown record id')
	} else if(is.na(record_set$record)){
		stop('show_record requires record id not a concept id')		
	}
			
	# Get the record metadata and a single row for the record
	metadata <- load_record_metadata(record_set)$metadata
	index <- get_index()
	row <- index[match(record_set$record, index$zenodo_record_id),]
	
	# Print out a summary
	cat('\nRecord summary\n')
	cat(sprintf('Title: %s\n', metadata$title))
	
	surnames <- sapply(strsplit(metadata$authors$name, ','), '[', 1)
	cat(sprintf('Authors: %s\n', paste(surnames, collapse=', ')))

	cat(sprintf('Publication date: %s\n', format(row$publication_date, '%Y-%m-%d')))
	cat(sprintf('Record ID: %i;\nConcept ID: %i\n', row$zenodo_record_id, row$zenodo_concept_id))

	status <- metadata$access
	if(status == 'embargo' & row$dataset_embargo < Sys.time()){
		status <- 'open'
	}
	cat(sprintf('Status: %s\n', status))
	
	ext_files <- metadata$external_files
	if(! is.null(ext_files)){
		cat(sprintf('External files: %s\n', paste(ext_files$file, collapse=' ,')))
	}
	
	# Taxa reporting
	taxa <- metadata$taxa
	if(length(taxa) > 0){
		cat(sprintf('Taxa: %i taxa reported\n', nrow(taxa)))
	}

	# Locations reporting
	locs <- metadata$locations
	if(length(locs) > 0){
		cat(sprintf('Locations: %i locations reported\n', nrow(locs)))
	}
	
	# Data worksheets
	dwksh <- metadata$dataworksheets
	nm_nch <- max(nchar(dwksh$name))
	cl_nch <- max(ceiling(log10(dwksh$max_col)), 4)
	rw_nch <- max(ceiling(log10(dwksh$n_data_row)), 4)

	cat('\nData worksheets:\n')
	cat(sprintf('%*s %*s %*s %s', nm_nch, 'name', cl_nch, 'ncol', 
				rw_nch, 'nrow', 'description'), sep='\n')
	
	cat(with(dwksh, sprintf('%*s %*i %*i %s', nm_nch, name, cl_nch, max_col, 
							rw_nch, n_data_row, description)), sep='\n')
	cat('\n')
	return(invisible())
}

show_worksheet <- function(record_id, name){
	
	#' Show a summary for a data worksheet within a record.
	#'
	#' This function prints out summary information on a data worksheet
	#' contained in one the worksheets of SAFE dataset Excel file.
	#'
	#' The function return an error if a concept ID is provided.
	#' 
	#' @param record_id A reference to a SAFE record ID.
	#' @param name The name of the data worksheet
	#' @return NULL
	#' @export
	
	record_set <- validate_record_ids(record_id)
	
	if(nrow(record_set) != 1){
		stop('show_worksheet requires a single record id')
	}
	
	if(all(is.na(record_set))){
		stop('Unknown record id')
	} else if(is.na(record_set$record)){
		stop('show_worksheet requires record id not a concept id')
	}
			
	# Get the record metadata and a single row for the record
	metadata <- load_record_metadata(record_set)$metadata
	index <- get_index()
	row <- index[match(record_set$record, index$zenodo_record_id),]
		
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
	return(invisible())
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
