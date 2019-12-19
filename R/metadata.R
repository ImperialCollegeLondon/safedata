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
    #' Note that \code{record} will be NA when a value represents a concept id. Inputs
    #' that do not match a record or concept ids are returned in the attribute 
    #' \code{mismatches} of the record set.
    #'
    #' This function is largely used internally to validate user inputs and to provide
    #' a common output for the search functions but is exported to allow users to 
    #' check record ids and display summary information using the print method.
    #'
    #' @param record_set A vector of values containing Zenodo concept or record ids.
    #' @param x An object of class \code{safe_record_set}
    #' @param ... Further arguments to print methods, unused.
    #' @return An object of class \code{safe_record_set} (see Details)
    #' @examples
    #'    safedir <- system.file('example_data_dir', package='safedata')
    #'    set_safe_dir(safedir, update=FALSE, validate=FALSE)
    #'    validate_record_ids(c(3247631, 3266827, 3266821, -1000))
    #'    validate_record_ids(c('https://doi.org/10.5281/zenodo.3247631', 
    #'                            '10.5281/zenodo.3266827', 
    #'                            'https://zenodo.org/record/3266821',
    #'                            'not_this_one/3266821'))
    #' @aliases safe_record_set
    #' @export
    
    index <- load_index()
    
    # Only run validation if the input isn't already a record set
    if(! inherits(record_set, 'safe_record_set')){
    
        # Otherwise validate
        if(! (is.vector(record_set) && mode(record_set) %in% c('character', 'numeric'))){
            stop('record_set must be a character or numeric vector')
        }
    
        # store original versions
        names(record_set) <- record_set
        mismatches <- NULL
        
        if(mode(record_set) == 'numeric'){
        
            # If numbers, look for positive integers
            not_int <- record_set %% 1 != 0
            not_pos <- record_set <= 0
            valid <- (! not_int) & (! not_pos)

            if(any(! valid)){
                mismatches <- record_set[! valid]
                record_set <- record_set[valid]
                warning(paste0('Invalid numeric record ids: ', paste0(mismatches, collapse=',')))
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
                mismatches <- record_set[! valid]
                record_set <- record_set[valid]
                warning(paste0('Invalid character record ids: ', paste0(mismatches, collapse=',')))
                # Remove mismatches from the regexpr hits
                attr_match <- attributes(match)
                attr_match$match.length <- attr_match$match.length[valid]
                match <- match[valid]
                attributes(match) <- attr_match
            }
            
            # convert matching patterns to numeric
            record_set <- structure(as.numeric(regmatches(record_set, match)),
                                    names=record_set)
        }
        
        # Look for duplicates
        dupes <- duplicated(record_set)
        if(any(dupes)){
            duplicated_values <- record_set[dupes]
            record_set <- record_set[! dupes]
            warning(paste0('Duplicated record ids in record_set: ', paste0(duplicated_values, collapse=',')))
        }
        
        # Do they appear in the index as concept ids or record ids
        known <- record_set %in% c(index$zenodo_record_id, index$zenodo_concept_id)
    
        if(! all(known)){
            unknown <- record_set[! known]
            record_set <- record_set[known]
            warning(paste0('Unknown record ids in record_set: ', paste0(dupes, collapse=',')))
        }
            
        record_set <- data.frame(concept = ifelse(record_set %in% index$zenodo_concept_id, record_set, NA),
                                 record = ifelse(record_set %in% index$zenodo_record_id, record_set, NA))
    
        record_set$concept <- ifelse(is.na(record_set$concept), 
                                     index$zenodo_concept_id[match(record_set$record, index$zenodo_record_id)],
                                     record_set$concept)
        
         class(record_set) <- c('safe_record_set', 'data.frame')
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
    
    rownames(record_set) <- seq_along(record_set$record)
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
        
    cat(sprintf(msg, length(unique(x$concept)), n_records, 
                     n_status['*'], n_status['o'], n_status['x']))
    
    # This relies on the sort order set in validate_record_ids
    x$concept <- ifelse(duplicated(x$concept), '-------', x$concept)
    x$record <- ifelse(is.na(x$record), '-------', x$record)
    
    class(x) <- 'data.frame'
    print(subset(x, select=c(concept, record, available)))

    return(invisible())
}


fetch_record_metadata <- function(record_set){
    
    #' Get and load SAFE dataset metadata
    #'
    #' Internal handlers to i) ensure there are local copies of record metadata,
    #' fetching it from the SAFE project website if needed and ii) load that 
    #' data from file.
    #'
    #' This is the same metadata used to populate the Zenodo description but is 
    #' downloaded as JSON format and stored within the record directory in the
    #' SAFE data directory for reuse.
    #'
    #' @param record_set An object of class \code{\link{safe_record_set}}.
    #' @return The \code{fetch_record_metadata} function returns NULL and 
    #'    \code{load_record_metadata} returns a list object containing record metadata
    #' @describeIn fetch_record_metadata Download and store JSON metadata for a record
    #' @examples
    #'    \donttest{
    #'    safedir <- system.file('example_data_dir', package='safedata')
    #'    set_safe_dir(safedir, update=FALSE, validate=FALSE)
    #'    rec <- validate_record_ids(1400562)
    #'    safedata:::fetch_record_metadata(rec)
    #'    metadata <- safedata:::load_record_metadata(rec)
    #'    }
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
    
    #' @describeIn fetch_record_metadata Load JSON metadata for a record
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


show_concepts <- function(obj){
    
    #' Show SAFE dataset metadata
    #'
    #' These functions provide access to the metadata associated with SAFE
    #' datasets. The functions provide three levels of information: 
    #' \describe{
    #'      \item{\code{show_concepts}}{displays the record versions grouped under 
    #'          dataset concepts,}
    #'      \item{\code{show_record}}{displays summary information about a 
    #'            specific record, and }
    #'      \item{\code{show_worksheet}}{displays metadata about data worksheet
    #'            fields within a record.}
    #' }
    #' All three functions accept a first argument \code{obj}, which can be one 
    #' of three things:
    #' \enumerate{
    #'    \item A character or numeric vector of SAFE dataset records or concepts, 
    #'          which will be validated using \code{\link{validate_record_ids}}, or
    #'    \item An already validated \code{\link{safe_record_set}} object, or
    #'    \item A \code{safedata} data frame loaded using \code{\link{load_safe_data}}.
    #' }
    #' If \code{show_concepts} is passed a record id, then the function looks up the
    #' relevant concept. The version table indicates which versions are available ('*' 
    #' for the most recent available version and 'o' for older available versions),
    #' and which are unavailable due to embargo or retriction ('x').
    #'
    #' @param obj A reference to SAFE records or a loaded worksheet (see above)
    #' @param worksheet The name of a worksheet to show. Obviously, if \code{obj} 
    #'    is a loaded worksheet, that will be the worksheet described and this can
    #'    be left as NULL.
    #' @param extended_fields Logical - show a compact description of worksheet
    #'    fields or a longer output including full metadata descriptors.
    #' @return Invisibly, a SAFE metadata object or a list of such objects. These
    #'    are not really intended for end user consumption.
    #' @describeIn show_concepts Show the records associated with a dataset concept.
    #' @examples
    #'    safedir <- system.file('example_data_dir', package='safedata')
    #'    set_safe_dir(safedir, update=FALSE, validate=FALSE)
    #'    recs <- validate_record_ids(c(1400562, 3266827, 3266821))
    #'    show_concepts(recs)
    #'    show_record(recs[1,])
    #'    # Show worksheet metadata from a record or from a loaded worksheet
    #'    show_worksheet(1400562, 'EnvironVariables')
    #'    beetle_abund <- load_safe_data(1400562, 'Ant-Psel')
    #'    show_worksheet(beetle_abund, extended_fields=TRUE)
    #' @export
    
    if(inherits(obj, 'safedata')){
        record_set <- attr(obj, 'metadata')$safe_record_set
    } else {
        record_set <- validate_record_ids(obj)
    }
        
    # get the rows to report, sort by publication date and cut into record chunks
    index <- load_index()
    
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
        version_available[which(concept$most_recent_available)[1]] <- '*'
        
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
    
    if(inherits(obj, 'safedata')){
        record_set <- attr(obj, 'metadata')$safe_record_set
    } else {
        record_set <- validate_record_ids(obj)
    }
        
    if(nrow(record_set) != 1){
        stop('show_record requires a single record id')
    }
    
    if(is.na(record_set$record)){
        warning('Concept ID provided: showing available versions')
        show_concepts(record_set)
        return(invisible())        
    }
            
    # Get the record metadata and a single row for the record
    metadata <- load_record_metadata(record_set)
    
    # Print out a summary
    cat('\nRecord summary\n')
    cat(sprintf('Title: %s\n', metadata$title))
    
    surnames <- sapply(strsplit(metadata$authors$name, ','), '[', 1)
    cat(sprintf('Authors: %s\nPublication date: %s\n,Record ID: %i\nConcept ID: %i\n',
                paste(surnames, collapse=', '),
                format(as.POSIXct(metadata$publication_date), '%Y-%m-%d'),
                record_set$zenodo_record_id,
                record_set$zenodo_concept_id))

    status <- metadata$access
    if(status == 'embargo' && metadata$embargo_date < Sys.time()){
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
    return(invisible(metadata))
}


show_worksheet <- function(obj, worksheet=NULL, extended_fields=FALSE){

    #' @describeIn show_concepts Show details of a data worksheet
    #' @export
        
    if(inherits(obj, 'safedata')){
        record_set <- attr(obj, 'metadata')$safe_record_set
        worksheet <-  attr(obj, 'metadata')$name
    } else {
        record_set <- validate_record_ids(obj)
    
        if(nrow(record_set) != 1){
            stop('show_worksheet requires a single record id')
        }
    
        if(all(is.na(record_set))){
            stop('Unknown record id')
        } else if(is.na(record_set$record)){
            stop('show_worksheet requires record id not a concept id')
        }
        
        if(is.null(worksheet)){
            stop('No worksheet name supplied.')
        }
    }
        
    # Get the record metadata
    metadata <- load_record_metadata(record_set)
        
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
