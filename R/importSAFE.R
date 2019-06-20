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

createSafe <- function (summaryDf = NULL) {
  #' Initiate a new \code{safe_data} object
  #' 
  #' @param summaryDf
  #' @return
  #' @examples
  #' @export
  
  safeObj <- structure(list(), class = 'safe_data')
  
  if (is.null(summaryDf)) {
    return(safeObj)
  } else {
    safeObj <- processSafeSummary(safeObj, summaryDf)
    return(safeObj)
  }
}

is.safe_data <- function (obj) {
  #' Check if \code{obj} is of the \code{safe_data} class
  #' 
  #' @param obj
  #' @return Boolean
  #' @export
  
  if (class(obj) == 'safe_data') {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

processSafeSummary <- function (obj, summaryDf) {
  UseMethod('processSafeSummary', obj)
}

processSafeSummary.default <- function (obj, summaryDf) {
  stop('function processSafeSummary() works only with safe_data objects')
}

processSafeSummary.safe_data <- function (obj, summaryDf) {
  #' Process summary 'meta' information for SAFE project data
  #' 
  #' Adds summary information to a \code{safe_data} object using the dataframe
  #' provided. \code{processSafeSummary} reads and assigns variables from the
  #' summary dataframe - a transposed "Summary" worksheet from a SAFE project
  #' .xlsx dataset - into a \code{safe_data} object.
  #' 
  #' @param summaryDf Dataframe containing summary information associated with
  #'   the SAFE project dataset. This must be a "Summary" worksheet from a SAFE
  #'   file submission
  #' @return \code{safe_data} object with metadata added
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/summary/}
  #'   for information on the SAFE summary worksheet, \code{\link{createSafe}}
  #' @export
  
  obj$projectID <- summaryDf$SAFE.Project.ID[1]
  obj$title <- as.character(summaryDf$Title[1])
  obj$startDate <- as.Date.numeric(summaryDf$Start.Date[1], origin = '1899-12-30')
  obj$endDate <- as.Date.numeric(summaryDf$End.Date[1], origin = '1899-12-30')
  obj$workSheets <- gsub(' ', '', lapply(subset(as.character(
    summaryDf$Worksheet.name), !is.na(summaryDf$Worksheet.name)), simpleCap))
  for (i in 1:length(obj$workSheets)) {
    obj[[obj$workSheets[i]]] <- list()
  }

  return(obj)
}

print.safe_data <- function (obj) {
  #' Print summary of information for a \code{safe_data} object to the console
  #' 
  #' Prints a summary of metadata for the given SAFE project object to the
  #' command line. Includes the project title, project ID number, start and end
  #' dates, and data worksheet names.
  #' 
  #' @param obj A \code{safe_data} object
  #' @seealso \code{\link{createSafe}}, \code{\link{processSafeSummary}}
  #' @export
  
  cat('Project name:', obj$title, '\n')
  cat('Project ID:', obj$projectID, '\n')
  cat('Dates:', paste0(obj$startDate), 'to', paste(obj$endDate), '\n')
  cat('Contains', length(obj$workSheets), 'data worksheets:', '\n')
  cat('  ', paste0(obj$workSheets, collapse = ', '))
}

addTaxa <- function (obj, safeWorkbook) {
  UseMethod('addTaxa', obj)
}

addTaxa.default <- function (obj, safeWorkbook) {
  stop('function addTaxa() works only with safe_data objects')
}

addTaxa.safe_data <- function (obj, filePath) {
  #' Add taxonomic observations to a \code{safe_data} object
  #'
  #' This function adds taxonomic data from the Taxa worksheet in a SAFE project
  #' dataset - in the form of a table - to an existing \code{safe_data} object.
  #' If no Taxa worksheet is provided with the dataset, the table defaults to
  #' \code{NA}.
  #' 
  #' @param obj An existing \code{safe_data} object
  #' @param filePath Complete path to the SAFE project dataset (.xlsx format)
  #' @return A modified \code{safe_data} object with a \code{Taxa} dataframe
  #' @note Not all SAFE project submissions contain the Taxa worksheet (for
  #'   example if the data do not contain any taxonomic observations). In this
  #'   case \code{safe_data$Taxa} defaults to \code{NA}.
  #' @seealso \code{\link{buildTaxonHeirarchy}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/taxa/}
  #'   for information on the Taxa worksheet, \code{\link{addTaxonHeirarchies}}
  #' @export
  
  if (!grepl(".xlsx", filePath)) {
    stop(paste0('filePath must point to a SAFE .xlsx workbook! Please check the',
                ' path entered:', filePath))
  }
  
  obj$Taxa <- tryCatch(
    {
      as.data.frame(readxl::read_xlsx(filePath, 'Taxa', col_names = TRUE))
    },
    error = function(e) {
      warning('SAFE import note: No Taxa datasheet exists!')
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
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/safe_dataset_checker/gbif_validation/}
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
  #' @seealso \code{\link{getTaxonHeirarchy}}, \code{\link[rgbif]{name_backbone}}
  #' @export
  
  cols <- c('safeName', 'subspecies', 'species', 'genus', 'family', 'class', 
            'order', 'phylum', 'kingdom', 'matchType')
  taxaDf <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0), 
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
  UseMethod('addTaxonHeirarchies', obj)
}

addTaxonHeirarchies.default <- function (obj, ...) {
  stop('function addTaxonHeirarchies() works only with safe_data objects')
}

addTaxonHeirarchies.safe_data <- function (obj, ...) {
  #' Add complete taxonomic heirarchies to a \code{safe_data} object
  #' 
  #' Adds a dataframe of taxonomic heirarchies for all taxa reported in the SAFE
  #' dataset Taxa worksheet to the supplied \code{safe_data} object. When no
  #' Taxa worksheet exists, this function flags a warning.
  #' 
  #' @param obj An existing \code{safe_data} object
  #' @param ... Optional arguments to pass to \code{\link[rgbif]{name_backbone}}
  #' @return The updated \code{safe_data} object with a dataframe of taxonomic
  #'   heirarchies for all taxa listed in the Taxa worksheet of the SAFE dataset.
  #'   This is accessed using the reference \code{TaxonHeirarchy}
  #' @note Not all SAFE datasets contain the Taxa worksheet. In this case, a
  #'   value of \code{NA} will be assigned to the \code{TaxonHeirarchy}
  #' @seealso \code{\link{addTaxa}}, \code{\link{getNameBackbone}}
  #' @export
  
  if (is.null(obj$Taxa)){
    warning(paste0('Taxonomic data have not been added to the safe_data object,',
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

addLocations <- function (obj, ...) {
  UseMethod('addLocations', obj)
}

addLocations.default <- function (obj, ...) {
  stop('function addLocations() works only with safe_data objects')
}

addLocations.safe_data <- function (obj, filePath) {
  #' Add location information to a \code{safe_data} object
  #' 
  #' Processes the Locations worksheet in the SAFE dataset at the supplied
  #' \code{filePath} and adds it as a dataframe to an existing \code{safe_data}
  #' object (\code{obj}). When no Locations worksheet is found the function
  #' defaults to \code{NA}. The Locations worksheet contains details of the
  #' commonly used areas in which research was conducted at SAFE. In some cases
  #' locations will already be "known" (i.e. on the SAFE gazetteer at
  #' \url{https://www.safeproject.net/info/gazetteer}), in other cases they will
  #' be "new", and thus need to be accompanied by GPS coordinates.
  #' 
  #' @param obj An existing \code{safe_data} object
  #' @param filePath Complete file path to the SAFE dataset
  #' @return A modified \code{safe_data} with \code{Locations} dataframe added
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
  
  if (!grepl(".xlsx", filePath)) {
    stop(paste0('filePath must point to a SAFE .xlsx workbook! Please check the',
                ' path entered:', filePath))
  }
  
  obj$Locations <- tryCatch( {
      as.data.frame(readxl::read_xlsx(filePath, 'Locations', col_names = TRUE))
    },
    error = function(e) {
      warning('SAFE import note: No Locations datasheet exists!')
      return(NA)
    }
  )
  return(obj)
}

addData <- function (obj, filePath) {
  UseMethod('addData', obj)
}

addData.default <- function (obj, filePath) {
  stop('function addData() works only with safe_data objects')
}

addData.safe_data <- function (obj, filePath) {
  #' Add data worksheets to a \code{safe_data} object
  #' 
  #' This function processes data-containing worksheets for the SAFE dataset at
  #' the specified \code{filePath} and adds them to an existing \code{safe_data}
  #' object. \code{addData} creates a new dataframe within the \code{safe_data}
  #' object for each worksheet stored in the SAFE dataset. The dataframes are
  #' named as per those used in the SAFE dataset submission file. All data
  #' columns are imported according to the \code{field_type} provided in the
  #' SAFE data table. Header information for each worksheet is stored separately
  #' as a \code{metaInfo} attribute accessed via the dataframe.
  #' 
  #' @param obj An existing \code{safe_data} object
  #' @param filePath Complete path to the SAFE dataset, which is assumed to be
  #'   an .xlsx file format
  #' @return A modified \code{safe_data} object with data worksheets added 
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/}
  #'   for information on SAFE data worksheets
  #' @examples
  #'   # create a safe_data object and add data
  #'   filePath <- 'C:/Users/User/safe_data/path_to_file.xlsx'
  #'   summaryInfo <- readTransposedXlsx(filePath, sheetName='Summary')
  #'   safe <- createSafe(summaryInfo)
  #'   safe <- addData(safe, filePath)
  #'   
  #'   # access the data table named "data_1"
  #'   View(safe$data_1)
  #'   
  #'   # access the metadata for this table
  #'   View(attr(safe$data_1, 'metaInfo'))
  #' @export
  
  if (!grepl(".xlsx", filePath)) {
    stop(paste0('filePath must point to a SAFE .xlsx workbook! Please check the',
                ' path entered:', filePath))
  }
  
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
    obj[[obj$workSheets[i]]] <- data
    attr(obj[[obj$workSheets[i]]], 'metaInfo') <- headerInfo
  }
  
  return(obj)
}

importSafe <- function (filePath) {
  #' Import a SAFE data file
  #' 
  #' Create a new \code{safe_data} object with minimal summary information using
  #' the SAFE dataset located at the specified \code{filePath}. Note that
  #' currently only '.xlsx' file formats are supported - see
  #' \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/overview/#excel-format-overview}
  #' for more information.
  #' 
  #' @param filePath Full path to the .xlsx SAFE file
  #' @return A \code{safe_data} object with \code{Summary} information added
  #' @seealso \code{\link{getSafe}} for downloading SAFE files from the Zenodo
  #'   cloud database, \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/overview/#excel-format-overview}
  #'   for an overview on Excel file formats used for SAFE data submissions
  #' @export
  
  if (tools::file_ext(filePath) != 'xlsx') {
    stop(paste0('Supplied file extension is ', tools::file_ext(filePath), 
                ': currently only .xlsx format is supported'))
  } else {
    message(paste0('Opening file ', basename(filePath), '...'), appendLF = FALSE)
    summary <- readTransposedXlsx(filePath, sheetName='Summary')
    obj <- createSafe(summary)
    message(' completed!')
    return(obj)  
  }
}
