
show_concepts <- function(obj){
	
	#' Show SAFE dataset metadata
	#'
	#' These functions provide access to the metadata associated with SAFE
	#' datasets. The functions provide three levels of information: 
	#' \describe{
	#'	  \item{\code{show_concepts}}{displays the record versions grouped under 
	#'          dataset concepts,}
	#'	  \item{\code{show_record}}{displays summary information about a 
	#'	        specific record, and }
	#'	  \item{\code{show_worksheet}}{displays metadata about data worksheet
	#'	        fields within a record.}
	#' }
	#' All three functions accept a first argument \code{obj}, which can be one 
	#' of three things:
	#' \enumerate{
	#'    \item A character or numeric vector of SAFE dataset records or concepts, 
	#'          which will be validated using \code{validate_record_ids}, or
	#'    \item An already validated \code{safe_record_set} object, or
	#'    \item A \code{safedata} data frame loaded using \code{load_safe_data}.
	#' }
	#' is not itself a concept id, then the function looks up the relevant 
	#' concept. The version table indicates which versions are available ('<<<' 
	#' for the most recent available version and 'o' for older available versions),
	#' and which are unavailable due to embargo or retriction ('x').
	#'
	#' @param obj A reference to SAFE records or a loaded worksheet (see above)
	#' @param worksheet The name of a worksheet to show. Obviously, if \code{obj} 
	#'    is a loaded worksheet, that will be the worksheet described and this can
	#'    be left as NULL.
	#' @return Invisibly, a SAFE metadata object or a list of such objects. These
	#'    are not really intended for end user consumption.
	#' @describeIn show_concepts Show the records associated with a dataset concept  
	#' @export
	
	if(inherits(obj, 'safe_data')){
		record_set <- attr(obj, 'safe_data')$safe_record_set
	} else {
		record_set <- validate_record_ids(obj)
	}
		
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


show_record <- function(obj){

	#' @describeIn show_concepts Show details of a specific dataset
	#' @export
	
	if(inherits(obj, 'safe_data')){
		record_set <- attr(obj, 'safe_data')$safe_record_set
	} else {
		record_set <- validate_record_ids(obj)
	}
		
	if(nrow(record_set) != 1){
		stop('show_record requires a single record id')
	}
	
	if(all(is.na(record_set))){
		stop('Unknown record id')
	} else if(is.na(record_set$record)){
		stop('show_record requires record id not a concept id')		
	}
			
	# Get the record metadata and a single row for the record
	metadata <- load_record_metadata(record_set)
	
	# Print out a summary
	cat('\nRecord summary\n')
	cat(sprintf('Title: %s\n', metadata$metadata$title))
	
	surnames <- sapply(strsplit(metadata$metadata$authors$name, ','), '[', 1)
	cat(sprintf('Authors: %s\nPublication date: %s\n,Record ID: %i\nConcept ID: %i\n',
				paste(surnames, collapse=', '),
				format(as.POSIXct(metadata$publication_date), '%Y-%m-%d'),
				record_set$zenodo_record_id,
				record_set$zenodo_concept_id))

	status <- metadata$metadata$access
	if(status == 'embargo' && metadata$metadata$embargo_date < Sys.time()){
		status <- 'open'
	}
	cat(sprintf('Status: %s\n', status))
	
	ext_files <- metadata$metadata$external_files
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
	dwksh <- metadata$metadata$dataworksheets
	nm_nch <- max(nchar(dwksh$name))
	cl_nch <- max(ceiling(log10(dwksh$max_col)), 4)
	rw_nch <- max(ceiling(log10(dwksh$n_data_row)), 4)

	cat('\nData worksheets:\n')
	cat(sprintf('%*s %*s %*s %s', nm_nch, 'name', cl_nch, 'ncol', 
				rw_nch, 'nrow', 'description'), sep='\n')
	
	cat(with(dwksh, sprintf('%*s %*i %*i %s', nm_nch, name, cl_nch, max_col, 
							rw_nch, n_data_row, description)), sep='\n')
	cat('\n')
	return(invisible(metadata$metadata))
}


show_worksheet <- function(obj, worksheet, extended_fields=FALSE){

	#' @describeIn show_concepts Show details of a data worksheet
	#' @export
		
	if(inherits(obj, 'safe_data')){
		record_set <- attr(obj, 'safe_data')$safe_record_set
		worksheet <-  attr(obj, 'safe_data')$worksheet
	} else {
		record_set <- validate_record_ids(record_id)
	
		if(nrow(record_set) != 1){
			stop('show_worksheet requires a single record id')
		}
	
		if(all(is.na(record_set))){
			stop('Unknown record id')
		} else if(is.na(record_set$record)){
			stop('show_worksheet requires record id not a concept id')
		}
	}
		
	# Get the record metadata
	metadata <- load_record_metadata(record_set)$metadata
		
	# Find the worksheet
	dwksh <- metadata$dataworksheets
	idx <- which(dwksh$name == worksheet)
	if(! length(idx)){
		stop('Data worksheet not found. Worksheets available are: ', 
			 paste(dwksh$name, collapse=','))
	}
	
	dwksh <- dwksh[idx,]
	
	# Print out a summary
	cat(sprintf('Record ID: %i\n', metadata$zenodo_record_id))
	cat(sprintf('Worksheet name: %s\n', dwksh$name))
	cat(sprintf('Number of data rows: %s\n', dwksh$n_data_row))
	cat(sprintf('Number of data fields: %s\n', dwksh$max_col - 1))
	cat(sprintf('Description:\n%s\n', dwksh$description))

	if(metadata$access == 'embargo'){
		embargo_date <- as.POSIXct(metadata$embargo_date)
		if(embargo_date >= Sys.time()){
			cat(sprintf('Data embargoed until %s, only metadata available\n', 
				    	format(embargo_date, '%Y-%m-%d')))
		}
	} else if(metadata$access == 'restricted') {
		cat('Dataset restricted , only metadata available\n') 
	}

	cat('\nFields:\n')
	fields <- dwksh['fields'][[1]][[1]]

	if(extended_fields){
		# print a long list of fields and non NA descriptor metadata
		for(field_idx in seq_along(fields$field_name)){
			fld <- fields[field_idx,]
			cat(fld$field_name, ':\n')
			other_descriptors <- subset(fld, select=-c(field_name, col_idx, range))
			descriptor_string <- paste0(' - ', names(other_descriptors), ': ', other_descriptors)
			cat(descriptor_string[!is.na(other_descriptors)], sep='\n')
		}
		cat('\n')
	} else {
		# print a table of name, type and description (truncated to width)
		descriptors <- subset(fields, select=c(field_name, field_type, description))
		
		nc <- apply(descriptors, 2, nchar)
		max_desc <- options('width')$width - (max(rowSums(nc[, 1:2])) + 8)
		
		descriptors$description <- ifelse(nc[, 3] < max_desc, 
										  descriptors$description,
										  paste0(substr(descriptors$description, 0, max_desc - 3), "..."))		
		
		print(descriptors, right=FALSE)
	}
	return(invisible(metadata))
}
