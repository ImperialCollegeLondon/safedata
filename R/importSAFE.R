

get_taxonomy <- function(record_id){
    
    #' Get a data frame containing the taxonomy for a dataset
    #'
    #' The record metadata from the SAFE Project website API includes
    #' the taxon index for the dataset. This function turns that set of entries
	#' into a data frame, containing the backbone taxonomy used within GBIF and
	#' the taxon name used within the dataset to make it easy to merge onto data 
	#' tables from the dataset. 
	#'
	#' Note that the taxon name used in the dataset is not necessarily the same
	#' as the canonical GBIF ID for the taxon.
    #'
    #' Note that the \code{ape} function \code{as.phylo.formula} can be used
    #' to turn this structure into a 'phylogeny'.
    #' @param record_id A SAFE dataset record id 
    #' @return A dataframe of the taxa used in the dataset, with columns
	#'   giving the GBIF backbone taxonomic levels (kingdom, phylum, order, class, 
	#'   family, genus, species) along with the original taxon names used in the
	#'   dataset.
    #' @export
    
    taxa <- get_record_metadata(record_id)$taxa
    
    if(length(taxa) == 0){
        return(NA)
    } else {
        taxa <- data.frame(taxa, stringsAsFactors=FALSE)
        names(taxa) <- c('id','pid','name','level','status','asname','aslevel')
        
        # Note which rows are leaves - their ID does not appear in the parent ID column
        taxa$leaf <- ! taxa$id %in% taxa$pid
        total_leaves <- sum(taxa$leaf)
        
        # Generate the asname column - this should merge on to the taxon names
        # provided in the dataset.
        taxa$asname <- ifelse(is.na(taxa$asname), taxa$name, taxa$asname)
        
        # Divide the data frame up into a list giving the descendants from each parent id
        taxa <- split(as.data.frame(taxa), f=taxa[,2])
        
        # Now, starting with the parent index of -1 for the root, work through that
        # list of descendants. Get a set of descdendants, add any leaves to the final
        # output and then split what is left into the current top and remaining taxa.
        # These sets are stored in a stack with more specific taxa getting pushed onto
        # the top at index 1. The current name at each level is used as the stack list
        # name, so that names(stack) gives the current hierarchy.
        
        # start the stack off
        push <- taxa[["-1"]]
        stack <- list(list(top=push[1,], up_next=push[-1,]))
        names(stack) <- stack[[1]]$top$name
        
        # create a leaf table to fill in
        leaf_table <- matrix(NA, ncol=8, nrow=total_leaves)
        leaf_idx <- 0
        
        while(length(stack)){

            if(stack[[1]]$top$leaf){
                # If the top of the stack is a leaf, write it out into the leaf table
                hierarchy <- c(rev(names(stack)), rep(NA, 7 - length(stack)))
                leaf_idx <- leaf_idx + 1
                leaf_table[leaf_idx, ] <- c(hierarchy, stack[[1]]$top$asname)
                
                # Now work back down the stack to find the next level with something
                # left in the up_next slot.
                while(length(stack)){
                    push <- stack[[1]]$up_next
                    if(nrow(push)){
                        # ii) If there is anything left in up_next, bring one up and replace the stack top
                        push <- list(list(top=push[1,], up_next=push[-1,]))
                        names(push) <- push[[1]]$top$name
                        stack <- c(push , stack[-1])
                        break
                    } else {
                        # iii) Otherwise pop off the top of the stack
                        stack <- stack[-1]
                    }
                }
            } else {
                # Otherwise, if the top of the stack isn't a leaf,  stick its descendants onto the stack
                push <- taxa[[stack[[1]]$top$id]]
                push <- list(list(top=push[1,], up_next=push[-1,]))
                names(push) <- push[[1]]$top$name
                stack <- c(push , stack)
            }
        }

        leaf_table <- as.data.frame(leaf_table, stringsAsFactors=FALSE)
        names(leaf_table) <- c("kingdom", "phylum", "class", "order", "family", "genus", "species", "taxon_name")
        return(leaf_table)
    }
}

getSafeDataClassType <- function (safeType) {
  #' Get the R data type from the SAFE \code{field_type} variable
  #' 
  #' Takes a SAFE \code{field_type} variable and returns the appropriate ReadXl
  #' data type. This function is handy for ensuring SAFE worksheet data tables
  #' are correctly imported by ReadXl (i.e. that format types are consistent).
  #' If the data type is not identifiable then this function will return a value
  #' of "guess".
  
  #' @param safeType The SAFE \code{field_type} variable
  #' @return The corresponding readxl data type
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/}
  #'   for accepted SAFE data field types and 
  #'   \url{https://cran.r-project.org/web/packages/readxl/} for ReadXl data types
  #' @export
  
  typeDate <- c('Date','Datetime', 'Time')
  typeText <- c('Location', 'ID', 'Taxa', 'Replicate', 'Abundance')
  typeNum <- c('Latitude', 'Longitude', 'Numeric')
  typeFac <- c('Categorical', 'Ordered Categorical', 'Categorical Trait',
               'Numeric Trait', 'Categorical Interaction', 'Numeric Interaction')
  
  if (safeType %in% typeDate) {
    return('date')
  } else if (safeType %in% typeText) {
    return('text')
  } else if (safeType %in% typeNum) {
    return('numeric')
  } else if (safeType %in% typeFac) {
    return('text')
  } else {
    return('guess')
  }
}

isSafeTypeCategorical <- function (safeType) {
  #' Check if the SAFE data type is of R type \code{categorical}/\code{factor}
  #' 
  #' Takes a SAFE \code{field_type} and checks whether it is of R type
  #' "categorical"/"factor". This is used to convert SAFE data variables after
  #' import.
  #' 
  #' @param safeType The SAFE \code{field_type} variable
  #' @return \code{TRUE} if given variable is a categorical, \code{FALSE} if not
  #' @seealso \code{\link{getSafeDataClassType}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/}
  #'   for differernt SAFE data types
  #' @export
  
  if (safeType %in% c('Categorical', 'Ordered Categorical', 'Categorical Trait',
                      'Numeric Trait', 'Categorical Interaction',
                      'Numeric Interaction')) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

createSafe <- function (filePath = NULL, processSummary = TRUE) {
  #' Initiate a new \code{safedata} object
  #' 
  #' \code{create_safe} generates an object of class type \code{safedata} that
  #' is used to import and view SAFE project datasets.
  #' 
  #' @param filePath the complete path to the SAFE dataset .xlsx file. This 
  #'   could be absolute, or relative to the current working directory. When no
  #'   path is specified (i.e. \code{filePath = NULL}), an empty \code{safedata}
  #'   object is returned.
  #' @param processSummary a logical value indicating whether summary data
  #'   should be added to the \code{safedata} object on creation. If \code{TRUE},
  #'   then summary metadata is added to the object using the function
  #'   \code{\link{processSafeSummary}}.
  #' @return an object of class \code{safedata}.
  #' @seealso \code{\link{processSafeSummary}}
  #' @export
  
  obj <- structure(list(), class = 'safedata')
  if (is.null(filePath)) {
    warning('No filePath specified, returning empty safedata object',
            call. = FALSE, immediate. = TRUE)
    obj$filePath <- NULL
    return(obj)
  } else {
    if (!grepl(".xlsx", filePath)) {
      stop(paste0('filePath must point to a SAFE .xlsx workbook! Please check ',
                  'the path entered:', filePath))
    } else {
      obj$filePath <- filePath
      if (processSummary) {
        obj <- processSafeSummary(obj)
      }
    }
    return(obj)
  }
}


processSafeSummary <- function (obj, filePath = NULL) {
  #' Process summary 'meta' information for SAFE project data
  #' 
  #' Adds summary information to a \code{safedata} object using the dataframe
  #' provided. \code{processSafeSummary} reads and assigns variables from the
  #' summary dataframe - a transposed "Summary" worksheet from a SAFE project
  #' .xlsx dataset - into a \code{safedata} object.
  #' 
  #' @param obj An object of class \code{safedata}.
  #' @param filePath The complete path to the SAFE dataset .xlsx file. By default this
  #'   value is set to \code{NULL} and the function attempts to access the
  #'   \code{obj$filePath} variable (i.e. the file pointer stored in the
  #'   \code{safedata} object itself). When a \code{filePath} is supplied, it
  #'   must point to a SAFE .xlsx file, otherwise an error is returned.
  #' @return \code{safedata} object with metadata added.
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/summary/}
  #'   for information on the SAFE summary worksheet, \code{\link{createSafe}}
  #' @export
  
  # check file path
  if (is.null(filePath)) {
    if (!is.null(obj$filePath)) {
      filePath <- obj$filePath
    } else {
      stop(paste0('filePath not specified! Please check obj$filePath or pass a',
                  ' valid path to a SAFE dataset'))
    }
  } else {
    if (!grepl(".xlsx", filePath)) {
      stop(paste0('filePath must point to a SAFE .xlsx workbook! Please check ', 
                  'the path entered:', filePath))
    }
  }
  summary <- readTransposedXlsx(filePath, sheetName='Summary')
  
  # add summary data
  obj$projectID <- summary$SAFE.Project.ID[1]
  obj$title <- as.character(summary$Title[1])
  obj$startDate <- as.Date.numeric(summary$Start.Date[1], origin = '1899-12-30')
  obj$endDate <- as.Date.numeric(summary$End.Date[1], origin = '1899-12-30')
  obj$workSheets <- gsub(' ', '', lapply(subset(as.character(
    summary$Worksheet.name), !is.na(summary$Worksheet.name)), simpleCap))
  for (i in 1:length(obj$workSheets)) {
    obj[[obj$workSheets[i]]] <- 
      list("description" = as.character(summary$Worksheet.description[i]))
  }

  return(obj)
}

print.safedata <- function (x, ...) {
  #' Print summary of information for a \code{safedata} object to the console
  #' 
  #' Prints a summary of metadata for the given SAFE project object to the
  #' command line. Includes the project title, project ID number, start and end
  #' dates, and data worksheet names.
  #' 
  #' @param x An object of class \code{safedata}
  #' @param ... Further arguments to print generic, ignored
  #' @seealso \code{\link{createSafe}}, \code{\link{processSafeSummary}}
  #' @export
  
  cat('Project name:', x$title, '\n')
  cat('Project ID:', x$projectID, '\n')
  cat('Dates:', paste0(x$startDate), 'to', paste(x$endDate), '\n')
  cat('Contains', length(x$workSheets), 'data worksheet(s):', '\n')
  cat('  ', paste0(x$workSheets, collapse = ', '))
}


addTaxa <- function (obj, filePath = NULL) {
  #' Add taxonomic observations to a \code{safedata} object
  #'
  #' This function adds taxonomic data from the Taxa worksheet in a SAFE project
  #' dataset - in the form of a table - to an existing \code{safedata} object.
  #' If no Taxa worksheet is provided with the dataset, the table defaults to
  #' \code{NA}.
  #' 
  #' @param obj An existing object of class \code{safedata}
  #' @param filePath The complete path to the SAFE dataset .xlsx file. By 
  #'   default this value is set to \code{NULL} and the function attempts to 
  #'   access the \code{obj$filePath} variable (i.e. the file pointer stored in
  #'   the \code{safedata} object itself). When a \code{filePath} is supplied, 
  #'   it must point to a SAFE .xlsx file, otherwise an error is returned.
  #' @return A modified \code{safedata} object with a \code{Taxa} dataframe
  #' @note Not all SAFE project submissions contain the Taxa worksheet (for
  #'   example if the data do not contain any taxonomic observations). In this
  #'   case \code{safedata$Taxa} defaults to \code{NA}.
  #' @seealso \code{\link{addTaxonHeirarchies}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/taxa/}
  #'   for information on the Taxa worksheet, \code{\link{addTaxonHeirarchies}}
  #' @export
  
  if(! inherits(obj, 'safedata')){
      stop("addTaxa requires an object of class 'safedata'")
  }
  
  # check file path
  if (is.null(filePath)) {
    if (!is.null(obj$filePath)) {
      filePath <- obj$filePath
    } else {
      stop(paste0('filePath not specified! Please check obj$filePath or pass a',
                  ' valid path to a SAFE dataset'))
    }
  } else {
    if (!grepl(".xlsx", filePath)) {
      stop(paste0('filePath must point to a SAFE .xlsx workbook! Please check ', 
                  'the path entered:', filePath))
    }
  }
  
  # get Taxa
  obj$Taxa <- tryCatch(
    {
      as.data.frame(readxl::read_xlsx(filePath, 'Taxa', col_names = TRUE))
    },
    error = function(e) {
      warning('SAFE import note - No Taxa datasheet exists!',
              call. = FALSE, immediate. = TRUE)
      return(NA)
    }
  )
  return(obj)
}

getNameBackbone <- function(taxaRow, ...) {
  #' Return taxonomic heirarchy for a SAFE taxonomic entry
  #' 
  #' This function attempts to perform a search of the GBIF database for the
  #' named Taxon/Parent types in the SAFE project Taxa worksheet. The purpose
  #' of this is to construct a taxonomic tree for all taxa reported in the
  #' dataset(s) to facilitate data searching/subsetting.
  #' 
  #' @param taxaRow A row from a SAFE object Taxa worksheet
  #' @param ... Optional arguments to pass to \code{\link[rgbif]{name_backbone}}
  #' @return GBIF name backbone list
  #' 
  #' @seealso \code{\link[rgbif]{name_backbone}},
  #'   \code{\link[rgbif]{name_usage}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/taxa/},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/safedataset_checker/gbif_validation/}
  #'   
  #' @note This function can take a few seconds to run, particularly if there
  #'   are a large number of Taxa in the SAFE dataset.
  #' 
  #' @export
  
  if (!is.na(taxaRow[['Parent name']])) {
    # The Parent Name has been provided. This means one of 3 things:
    # 1. The Taxon type is a morphospecies or functional group
    # 2. The Taxon name:type is unrecognized
    # 3. The Taxon is from a less common taxonomic level
    # In any case, taxonomic look-ups need to be done at the Parent level
    level = 'Parent'
  } else {
    # No Parent Name given so look-ups can be done on the provided Taxon
    level = 'Taxon'
  }
  
  # Taxonomic look-ups then follow a multi-step process
  # 1. strict search on the name backbone
  nameBackbone <- rgbif::name_backbone(name = taxaRow[[paste0(level, ' name')]],
                                       rank = taxaRow[[paste0(level, ' type')]], 
                                       strict = TRUE, verbose = FALSE, ...)
  
  # 2. if this search fails there are a few options
  if (nameBackbone$matchType == 'NONE') {
    # (i) attempt to match on Taxon/Parent ID (if this has been provided)
    # this is the case when there are multiple entries for the given name
    # first get all alternatives of the name using a strict search
    if (!is.na(taxaRow[[paste(level, 'ID')]])) {
      altOpts <- rgbif::name_backbone(name = taxaRow[[paste0(level, ' name')]],
                                      rank = taxaRow[[paste0(level, ' type')]], 
                                      strict = TRUE, verbose = TRUE, 
                                      ...)$alternatives
      # then match the ID against the GBIF entry within the alternatives
      nameBackbone <- altOpts[altOpts$usageKey==taxaRow[[paste0(level, ' ID')]],]
    } else {
      # (ii) when no Taxon/Parent ID is given, perform non-strict GBIF search
      altOpts <- rgbif::name_backbone(name = taxaRow[[paste0(level, ' name')]],
                                      rank = taxaRow[[paste0(level, ' type')]], 
                                      strict = FALSE, verbose = TRUE, ...)
      if (altOpts$data$matchType != 'NONE') {
        # take the top-matched entry, which may be a higher-rank
        nameBackbone <- altOpts$data
      } else {
        # in rare cases this still fails, so take the alternative GBIF search
        # option that has the highest confidence value
        nameBackbone <- altOpts$alternatives[
          which.max(altOpts$alternatives$confidence),]
      }
    }
  }
  
  nameBackbone$safeName <- taxaRow[['Name']]
  return(nameBackbone)
}

nameBackboneToDf <- function (nameBackbone) {
  #' Convert RGBIF name_backbone list to dataframe
  #' 
  #' For improved readability and searchability, this function converts an
  #' \code{rgbif} name backbone (list) to a dataframe, storing information for
  #' the 8 main taxonomic levels (subspecies, species, genus, family, class,
  #' order, phylum, kingdom). The dataframe also includes the unique taxon
  #' identifier within the SAFE project submission and the \code{matchtype}
  #' variable returned from \code{rgbif}.
  #' 
  #' @param nameBackbone List of taxonomic heirarchy returned by 
  #'   \code{\link[rgbif]{name_backbone}}
  #' @return Taxonomic heirarchy stored as a dataframe including the 8 main
  #'   taxonomic levels
  #' @seealso \code{\link{addTaxonHeirarchies}}, \code{\link[rgbif]{name_backbone}}
  #' @export
  
  cols <- c('safeName', 'subspecies', 'species', 'genus', 'family', 'class', 
            'order', 'phylum', 'kingdom', 'matchType')
  taxaDf <- stats::setNames(data.frame(matrix(ncol = length(cols), nrow = 0), 
                                stringsAsFactors = FALSE), cols)
  rgbifDf <- as.data.frame(nameBackbone, stringsAsFactors = FALSE)
  return(dplyr::bind_rows(rgbifDf, taxaDf)[, cols])
}

getTaxonWrapper <- function (taxaRow, ...) {
  #' Extract taxonomic heirarchy from the GBIF database for the given taxa
  #' 
  #' This function is a wrapper that provides capabilities to extract taxonomic
  #' herirarchies from the GBIF database using Taxa worksheet in a SAFE data 
  #' file.
  #'
  #' @param taxaRow A row from a SAFE object Taxa worksheet
  #' @param ... Optional arguments to pass to \code{\link[rgbif]{name_backbone}}
  #' @return Dataframe containing the taxonomic heirarchy for the given taxon
  #' @seealso \code{\link{getNameBackbone}}, \code{\link{nameBackboneToDf}}
  #' @export
  
  listBackbone <- getNameBackbone(taxaRow, ...)
  return(nameBackboneToDf(listBackbone))
}

addTaxonHeirarchies <- function (obj, ...) {
  #' Add complete taxonomic heirarchies to a \code{safedata} object
  #' 
  #' Adds a dataframe of taxonomic heirarchies for all taxa reported in the SAFE
  #' dataset Taxa worksheet to the supplied \code{safedata} object. When no
  #' Taxa worksheet exists, this function flags a warning.
  #' 
  #' @param obj An existing object of class \code{safedata}
  #' @param ... Optional arguments to pass to \code{\link[rgbif]{name_backbone}}
  #' @return The updated \code{safedata} object with a dataframe of taxonomic
  #'   heirarchies for all taxa listed in the Taxa worksheet of the SAFE dataset.
  #'   This is accessed using the reference \code{TaxonHeirarchy}
  #' @note Not all SAFE datasets contain the Taxa worksheet. In this case, a
  #'   value of \code{NA} will be assigned to the \code{TaxonHeirarchy}
  #' @seealso \code{\link{addTaxa}}, \code{\link{getNameBackbone}}
  #' @export
  
  if(! inherits(obj, 'safedata')){
      stop("addTaxonHeirarchies requires an object of class 'safedata'")
  }
  
  if (is.null(obj$Taxa)){
    warning(paste0('Taxonomic data have not been added to the safedata object,',
                   ' please see addTaxa() function'))
  } else if (is.na(obj$Taxa)){
    warning('Cannot process Taxa: SAFE dataset contains no Taxa worksheet!')
    obj$TaxonHeirarchy <- NA
  } else {
    startT <- Sys.time()
    message('Starting taxonomy look-up... ', appendLF = FALSE)
    obj$TaxonHeirarchy <- 
      invisible(dplyr::bind_rows(apply(obj$Taxa, 1, getTaxonWrapper)))
    runT <- difftime(Sys.time(), startT, units = 'sec')
    message(sprintf('completed! This took %.2f seconds!', runT))
  }
  return(obj)
}

addLocations <- function (obj, filePath = NULL) {
  #' Add location information to a \code{safedata} object
  #' 
  #' Processes the Locations worksheet in the SAFE dataset at the supplied
  #' \code{filePath} and adds it as a dataframe to an existing \code{safedata}
  #' object (\code{obj}). When no Locations worksheet is found the function
  #' defaults to \code{NA}. The Locations worksheet contains details of the
  #' commonly used areas in which research was conducted at SAFE. In some cases
  #' locations will already be "known" (i.e. on the SAFE gazetteer at
  #' \url{https://www.safeproject.net/info/gazetteer}), in other cases they will
  #' be "new", and thus need to be accompanied by GPS coordinates.
  #' 
  #' @param obj An existing object of class \code{safedata}
  #' @param filePath The complete path to the SAFE dataset .xlsx file. By 
  #'   default this value is set to \code{NULL} and the function attempts to 
  #'   access the \code{obj$filePath} variable (i.e. the file pointer stored in
  #'   the \code{safedata} object itself). When a \code{filePath} is supplied, 
  #'   it must point to a SAFE .xlsx file, otherwise an error is returned.
  #' @return A modified \code{safedata} object with \code{Locations} added
  #' @note Although unusual, not all SAFE project submissions will contain a
  #'   Locations worksheet. Examples of this include projects that present only
  #'   laboratory data (that do not have the requirement of specifying where
  #'   field samples came from), or for inconsistent data collection locations
  #'   (e.g. tracking animal movements). In the case of the latter GPS data
  #'   should be provided separately for each observation.
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/locations/}
  #'   for details on the Locations worksheet,
  #'   \url{https://www.safeproject.net/info/gazetteer} for the SAFE gazetteer
  #' @export
  
  if(! inherits(obj, 'safedata')){
      stop("addLocations requires an object of class 'safedata'")
  }
  # check file path
  if (is.null(filePath)) {
    if (!is.null(obj$filePath)) {
      filePath <- obj$filePath
    } else {
      stop(paste0('filePath not specified! Please check obj$filePath or pass a',
                  ' valid path to a SAFE dataset'))
    }
  } else {
    if (!grepl(".xlsx", filePath)) {
      stop(paste0('filePath must point to a SAFE .xlsx workbook! Please check ', 
                  'the path entered:', filePath))
    }
  }
  
  # add Locations
  obj$Locations <- tryCatch( {
      as.data.frame(readxl::read_xlsx(filePath, 'Locations', col_names = TRUE))
    },
    error = function(e) {
      warning('SAFE import note - No Locations datasheet exists!', 
              call. = FALSE, immediate. = TRUE)
      return(NA)
    }
  )
  return(obj)
}

addData <- function (obj, filePath = NULL) {
  #' Add data worksheets to a \code{safedata} object
  #' 
  #' This function processes data-containing worksheets for the SAFE dataset at
  #' the specified \code{filePath} and adds them to an existing \code{safedata}
  #' object. \code{addData} creates a new dataframe within the \code{safedata}
  #' object for each worksheet stored in the SAFE dataset. The dataframes are
  #' named as per those used in the SAFE dataset submission file. All data
  #' columns are imported according to the \code{field_type} provided in the
  #' SAFE data table. Header information for each worksheet is stored separately
  #' as a \code{metaInfo} attribute accessed via the dataframe.
  #' 
  #' @param obj An existing object of class \code{safedata}.
  #' @param filePath The complete path to the SAFE dataset .xlsx file. By 
  #'   default this value is set to \code{NULL} and the function attempts to 
  #'   access the \code{obj$filePath} variable (i.e. the file pointer stored in
  #'   the \code{safedata} object itself). When a \code{filePath} is supplied, 
  #'   it must point to a SAFE .xlsx file, otherwise an error is returned.
  #' @return A modified \code{safedata} object with data worksheets added. 
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/}
  #'   for information on SAFE data worksheets
  #' @examples
  #'   # create a safedata object and add data
  #'   filePath <- system.file('extdata', 'demo_data.xlsx', package='safedata')
  #'   safe <- importSafe(filePath)
  #'   safe <- addData(safe, filePath)
  #'   
  #'   # access the data table named "data_1"
  #'   View(safe$data_1)
  #'   
  #'   # access the metadata for this table
  #'   View(attr(safe$data_1, 'metaInfo'))
  #' @export
  
  if(! inherits(obj, 'safedata')){
      stop("addData requires an object of class 'safedata'")
  }
  
  # check file path
  if (is.null(filePath)) {
    if (!is.null(obj$filePath)) {
      filePath <- obj$filePath
    } else {
      stop(paste0('filePath not specified! Please check obj$filePath or pass a',
                  ' valid path to a SAFE dataset'))
    }
  } else {
    if (!grepl(".xlsx", filePath)) {
      stop(paste0('filePath must point to a SAFE .xlsx workbook! Please check ', 
                  'the path entered:', filePath))
    }
  }
  
  # add data
  sheetNames <- readxl::excel_sheets(filePath)
  sheetNamesNorm <- gsub(' ', '', lapply(sheetNames, simpleCap))
  
  for (i in 1:length(obj$workSheets)) {
    idx <- which.max(sheetNamesNorm == obj$workSheets[i])
    
    # get line index where actual data begins (at 'field_name' header)
    fullData <- as.data.frame(suppressMessages(
      readxl::read_xlsx(
        filePath, sheetNames[idx], col_names = FALSE, na = c('', 'NA'))))
    firstDataRow <- which.max(fullData[,1] == 'field_name')
    
    # extract meta information from header lines
    headerInfo <- readTransposedXlsx(filePath, sheetNames[idx], na = c('', 'NA'),
                                     n_max = firstDataRow-1)
    fieldTypes <- c('numeric', sapply(headerInfo$field_type, getSafeDataClassType))
    
    # store actual data (without header info) 'en masse'
    data <- as.data.frame(suppressMessages(
      readxl::read_xlsx(filePath, sheetNames[idx], col_names = TRUE, 
                        col_types = fieldTypes, skip = firstDataRow-1, 
                        na = c('', 'NA'))))
    
    # convert categorical data types
    categoricals <- c(FALSE, sapply(headerInfo$field_type, isSafeTypeCategorical))
    factorCols <- names(data)[categoricals]
    data[factorCols] <- lapply(data[factorCols], factor)
    obj[[obj$workSheets[i]]]$data <- data
    attr(obj[[obj$workSheets[i]]]$data, 'metaInfo') <- headerInfo
  }
  
  return(obj)
}

importSafe <- function (filePath) {
  #' Import a SAFE data file
  #' 
  #' Create a new \code{safedata} object with minimal summary information using
  #' the SAFE dataset located at the specified \code{filePath}. Note that
  #' currently only '.xlsx' file formats are supported - see
  #' \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/overview/#excel-format-overview}
  #' for more information.
  #' 
  #' @param filePath Full path to the .xlsx SAFE file
  #' @return A \code{safedata} object with \code{Summary} information added
  #' @seealso \code{\link{getSafe}} for downloading SAFE files from the Zenodo
  #'   cloud database, \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/overview/#excel-format-overview}
  #'   for an overview on Excel file formats used for SAFE data submissions
  #' @export
  
  if (tools::file_ext(filePath) != 'xlsx') {
    stop(paste0('Supplied file extension is ', tools::file_ext(filePath), 
                ': currently only .xlsx format is supported'))
  } else {
    message(paste0('Opening file ', basename(filePath), '...'), appendLF = FALSE)
    obj <- createSafe(filePath)
    message(' completed!')
    return(obj)  
  }
}
