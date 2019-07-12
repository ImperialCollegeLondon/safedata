
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


validate_record_ids <- function(ids){
	
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
	#' @param ids A vector of values containing Zenodo concept or record ids.
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
	
	index <- get_index()
	
	# Only run validation if the input isn't already a record set
	if(! inherits(ids, 'safe_record_set')){
	
		# Otherwise validate
		if(! (is.vector(ids) && mode(ids) %in% c('character', 'numeric'))){
			stop('ids must be a character or numeric vector')
		}
	
		# store original versions
		user <- ids
		
		if(mode(ids) == 'numeric'){
		
			# If numbers, look for positive integers
			not_int <- ids %% 1 != 0
			not_pos <- ids <= 0
			valid <- (! not_int) & (! not_pos)

			if(any(! valid)){
				warning('Some record ids are not positive integers')
			}
				
		} else if(mode(ids) == 'character'){
		
			# If string look for one of the possible string representations
			# of the record: a straight string of the integer or
			# could be a DOI (possibly with URL) or a Zenodo URL
			# - https://www.zenodo.org/record/3247631#.XRreJ9NKgWo
			# - https://doi.org/10.5281/zenodo.3247631
			# - 10.5281/zenodo.3247631

			match <- regexpr('^[0-9]+$|(?<=record/)[0-9]+|(?<=zenodo.)[0-9]+', ids, perl=TRUE)
			valid <- match != -1
		
			if(any(! valid)){
				warning('Some record ids do not match known id formats')
			}
		
			out <- rep(NA,length(ids))
			out[match != -1] <- regmatches(ids, match)
			ids <- as.numeric(out)
			valid <- match != -1
		}
			
		# Do they appear in the index as concept ids or record ids
		known <- ids %in% c(index$zenodo_record_id, index$zenodo_concept_id)
	
		if(! all(known)){
			warning('Some values are not known concept or record ids')
		}
			
		record_set <- data.frame(concept = ifelse(ids %in% index$zenodo_concept_id, ids, NA),
								 record = ifelse(ids %in% index$zenodo_record_id, ids, NA))
	
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
	
	# Now add information on whether records are available and the most recent available for each concept
	# Options are : available and most recent, available and not most recent (outdated),
	# not available and most recent is not NA (embargoed copy with open version) and
	# not available and most recent is NA (embargoed or restricted copies only).
    record_set$available <- index$available[match(record_set$record, index$zenodo_record_id)]
	mra <- subset(index, most_recent_available, select=c(zenodo_concept_id, zenodo_record_id))
	record_set$mra <- mra$zenodo_record_id[match(record_set$concept, mra$zenodo_concept_id)]
	
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
	x$flag <- with(x, ifelse(! available, 'x', ifelse(record == mra, '*','o')))
	n_status <- c('*'=0, 'x'=0, 'o'=0)
	counts <- table(x$flag)
	n_status[names(counts)] <- counts
	n_records <- sum(!is.na(x$record))
		
	cat(sprintf(msg, nrow(x) - n_records, n_records, 
					 n_status['*'], n_status['o'], n_status['x']))
	
	concepts <- split(x, x$concept)	
	
	for(cn in concepts){
		# sort the records in order of zenodo record (should mirror publication order)
		# and put any concept IDs (record = NA) first
		cn <- cn[order(cn$record, decreasing=TRUE, na.last=FALSE),]
		concept_str <- ifelse(seq_along(cn$concept) == 1, cn$concept, '-------')
		record_str <- ifelse(is.na(cn$record), '-------', sprintf('%i (%s)', cn$record, cn$flag))
		
		
		cat(paste0(concept_str, " : ", record_str), sep='\n')
	}
	
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
		record_set$local_path <- with(record_set, file.path(safedir, concept, record, sprintf('%i.rds', record)))
		record_set$to_download <- ! file.exists(record_set$local_path)
		
		record_set <- subset(record_set, to_download)
		
		if(nrow(record_set)){
			verbose_message('Downloading ', nrow(record_set), ' record metadata files\n')
		}
		
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
