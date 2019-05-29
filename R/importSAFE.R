readTransposedXlsx <- function (file, sheetName, ...) {
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
    readxl::read_xlsx(file, sheet = sheetName, col_names = FALSE, ...))
  dfT <- as.data.frame(t(df[-1]), stringsAsFactors = FALSE)
  names(dfT) <- t(df[,1])
  dfT <- as.data.frame(lapply(dfT, type.convert))
  
  return(dfT)
}

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

getDataClass <- function (safeType) {
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

isCategorical <- function (safeType) {
  #' Check if the SAFE data type is of R type "categorical"/"factor"
  #' 
  #' Takes a SAFE \code{field_type} and checks whether it is of R type
  #' "categorical"/"factor". This is used to convert SAFE data variables
  #' after import.
  #' 
  #' @param safeType The SAFE \code{field_type} variable
  #' @return \code{TRUE} if given variable is a categorical, \code{FALSE} if not
  #' @seealso \code{\link{getDataClass}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/}
  #'   for differernt SAFE data types
  
  if (safeType %in% c('Categorical', 'Ordered Categorical', 'Categorical Trait',
                      'Numeric Trait', 'Categorical Interaction',
                      'Numeric Interaction')) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

processSummary <- function (df) {
  #' Process summary 'meta' information for SAFE project data
  #' 
  #' Initiates a SAFE data object from the summary sheet provided. This function
  #' reads and assigns variables from the provided summary dataframe (a
  #' transposed "Summary" worksheet from a SAFE project submission file) into a
  #' new SAFE \code{list} object.
  #' 
  #' @param dataframe Dataframe containing summary information. This must be a 
  #'   "Summary" worksheet from a SAFE project submission file
  #' @return SAFE List object containing summary information for the project
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/summary/}
  #'   for information on the SAFE summary worksheet
  
  safeObj <- list()
  safeObj$projectID <- df$SAFE.Project.ID[1]
  safeObj$title <- as.character(df$Title[1])
  safeObj$workSheets <- gsub(' ', '', lapply(subset(
    as.character(df$Worksheet.name), !is.na(df$Worksheet.name)), simpleCap))
  for (i in 1:length(safeObj$workSheets)) {
    safeObj[[safeObj$workSheets[i]]] <- list()
  }
  safeObj$startDate <- as.Date.numeric(df$Start.Date[1], origin = '1899-12-30')
  safeObj$endDate <- as.Date.numeric(df$End.Date[1], origin = '1899-12-30')
  
  return(safeObj)
}

printSummary <- function (safeObj) {
  #' Prints summary information for SAFE object
  #' 
  #' Prints a summary of metadata for the given SAFE project object to the
  #' command line. Includes the project title, project ID number, start and end
  #' dates, and data worksheet names.
  #' 
  #' @param safeObj Existing SAFE data object with meta information added
  #' @seealso \code{\link{processSummary}}
  #' @export
  
  
  cat('Project name:', safeObj$title, '\n')
  cat('Project ID:', safeObj$projectID, '\n')
  cat('Dates:', paste0(safeObj$startDate), 'to', paste(safeObj$endDate), '\n')
  cat('Contains', length(safeObj$workSheets), 'data worksheets:', '\n')
  cat('  ', paste0(safeObj$workSheets, collapse = ', '))
}

processTaxa <- function (file, safeObj) {
  #' Add taxa information to SAFE data object
  #'
  #' This function adds a new dataframe to an existing SAFE object containing
  #' all taxonomic information for the submitted dataset. Note that when the
  #' data do not contain any taxa this table defaults to \code{NA}. When
  #' provided, this dataframe is used to compile a complete taxonomic heirarchy 
  #' for all taxa identified within the dataset.
  #' 
  #' @param file Complete path to the SAFE project file
  #' @param safeObj An existing SAFE data object
  #' @return A modified SAFE object with a \code{Taxa} dataframe
  #' @note Not all SAFE project submissions contain the Taxa worksheet (for
  #'   example if the data do not contain taxa). In this case the SAFE Taxa
  #'   object defaults to \code{NA}.
  #' @seealso \code{\link{buildTaxonHeirarchy}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/taxa/}
  #'   for information on the Taxa worksheet
  
  safeObj$Taxa <- tryCatch(
    {
      as.data.frame(readxl::read_xlsx(file, 'Taxa', col_names = TRUE))
    },
    error = function(e) {
      warning('SAFE import note: No Taxa datasheet supplied')
      return(NA)
    }
  )
  return(safeObj)
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
  
  listBackbone <- getNameBackbone(taxaRow, ...)
  return(nameBackboneToDf(listBackbone))
}

addTaxonHeirarchies <- function (safeObj, ...) {
  #' Add taxonomic heirarchies to SAFE object
  #' 
  #' Adds a dataframe of taxonomic heirarchies for all taxa reported in the SAFE
  #' Taxa worksheet to the supplied SAFE object. When no Taxa worksheet has been
  #' supplied, this function flags a warning.
  #' 
  #' @param safeObj An existing SAFE data object
  #' @param ... Optional arguments to pass to \code{\link[rgbif]{name_backbone}}
  #' @return The updated SAFE object \code{safeObj} with a dataframe of
  #'   taxonomic heirarchies for all taxa listed in the Taxa worksheet of the
  #'   data file. This is accessed using the reference \code{TaxaTree}
  #' @note Not all SAFE data files contain the Taxa worksheet. In this case, a
  #'   value of \code{NA} will be assigned to the \code{TaxaTree}
  #' @seealso \code{\link{getNameBackbone}}
  
  startT <- Sys.time()
  message('Starting taxonomy look-up... ', appendLF = FALSE)
  safeObj$TaxaTree = tryCatch( {
      invisible(dplyr::bind_rows(apply(safeObj$Taxa, 1, getTaxonWrapper)))
    },
    error = function(e) {
      warning('Cannot process Taxa: SAFE project contains no Taxa worksheet!')
      return(NA)
    }
  )
  runT <- difftime(Sys.time(), startT, units = 'sec')
  message(sprintf('completed! This took %.2f seconds!', runT))
  return(safeObj)
}

processLocations <- function (file, safeObj) {
  #' Add location information to SAFE data object
  #' 
  #' Processes the \code{Locations} worksheet in the SAFE project file and adds
  #' it as a dataframe to the existing SAFE data object.
  #' 
  #' @param file Complete path to the SAFE object file
  #' @param safeObj An existing SAFE data object
  #' @return A modified SAFE object with a \code{Locations} dataframe
  #' @note Although unusual, not all SAFE project submissions will contain a
  #'   Locations worksheet. Examples of this include projects that present only
  #'   laboratory data (that do not have the requirement of specifying where
  #'   field samples came from), or for inconsistent data collection locations
  #'   (e.g. tracking animal movements). In the case of the latter GPS data
  #'   should be provided separately for each observation.
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/locations/}
  #'   for details on the Locations worksheet
  
  safeObj$Locations <- tryCatch( {
      as.data.frame(readxl::read_xlsx(file, 'Locations', col_names = TRUE))
    },
    error = function(e) {
      warning('SAFE import note: No Locations datasheet supplied')
      return(NA)
    }
  )
  return(safeObj)
}

processData <- function (file, safeObj) {
  #' Add data worksheets to SAFE project object
  #' 
  #' This function processes data-containing worksheets for the given SAFE
  #' project file and adds them to an existing SAFE data object. This function
  #' works by creating a new \code{dataframe} within the SAFE object for each
  #' worksheet found within the SAFE project file. The dataframes are named
  #' according to their names in the original SAFE project file and contain the
  #' data within the given workseet. All data columns are imported according to 
  #' the \code{field_type} provided in the SAFE data table. Header information
  #' for each worksheet is stored as a \code{metaInfo} attribute accessed via
  #' the dataframe.
  #' 
  #' @param file Path to the SAFE project file. This is assumed to be .xlsx
  #' @param safeObj An existing SAFE data object
  #' @return A modified SAFE data object containing data worksheets 
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/}
  #'   for information on SAFE data worksheets
  #' @note To access worksheet meta information in a SAFE object \code{safeObj}
  #'   with worksheet \code{data}, it is recommended to use
  #'   \code{View(attr(safeObj$data, 'metaInfo'))}
  
  sheets <- readxl::excel_sheets(file)
  sheetsNormed <- gsub(' ', '', lapply(sheets, simpleCap))
  
  for (i in 1:length(safeObj$workSheets)) {
    idx <- which.max(sheetsNormed == safeObj$workSheets[i])
    
    # get line index where actual data begins (at 'field_name' header)
    fullData <- as.data.frame(suppressMessages(
      readxl::read_xlsx(file, sheets[idx], col_names = FALSE, na = c('', 'NA'))))
    firstDataRow <- which.max(fullData[,1] == 'field_name')
    
    # extract meta information from header lines
    headerInfo <- readTransposedXlsx(fPath, sheets[idx], na = c('', 'NA'),
                                     n_max = firstDataRow-1)
    fieldTypes <- c('numeric', sapply(headerInfo$field_type, getDataClass))
    
    # store actual data (without header info) 'en masse'
    data <- as.data.frame(suppressMessages(
      readxl::read_xlsx(fPath, sheets[idx], col_names = TRUE, 
                        col_types = fieldTypes, skip = firstDataRow-1, 
                        na = c('', 'NA'))))

    # convert categorical data types
    categoricals <- c(FALSE, sapply(headerInfo$field_type, isCategorical))
    factorCols <- names(data)[categoricals]
    data[factorCols] <- lapply(data[factorCols], factor)
    safeObj[[safeObj$workSheets[i]]] <- data
    attr(safeObj[[safeObj$workSheets[i]]], 'metaInfo') <- headerInfo
  }
  
  return(safeObj)
}

importSAFE <- function (file) {
  #' Import a SAFE data file
  #' 
  #' This function is a wrapper to open and process SAFE data tables stored in
  #' memory as .xlsx files. A full path to a SAFE .xlsx file should be provided,
  #' which is then opened and processed according to the following procedure:
  #' 1. Process \code{Summary} worksheet
  #' 2. Process \code{Taxa} worksheet (when provided)
  #' 3. Process \code{Locations} worksheet (when provided)
  #' 4. Process individual data worksheets
  #' More information on SAFE .xlsx file structures can be found at
  #' \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/overview/#excel-format-overview}.
  #' 
  #' @param file Path to the .xlsx SAFE file
  #' @return A SAFE object \code{list} with varied attributes, including
  #'   \code{Summary}, \code{Taxa}, \code{Locations}, and individual data
  #'   fields for accessing different components of the SAFE record data.
  #' @seealso \code{\link{getSAFE}} for downloading SAFE files from the Zenodo
  #'   cloud database, \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/overview/#excel-format-overview}
  #'   for an overview on Excel file formats used for SAFE data submissions.
  #' @export
  
  if (tools::file_ext(file) != 'xlsx') {
    stop(paste0('File extension is ', tools::file_ext(file), ': currently only',
                ' .xlsx format is supported'))
  } else {
    message(paste0('Opening file ', basename(file), '...'), appendLF = FALSE)
    summary <- readTransposedXlsx(file, sheetName='Summary')
    safeObj <- processSummary(summary)
    safeObj <- processTaxa(file, safeObj)
    safeObj <- processLocations(file, safeObj)
    safeObj <- processData(file, safeObj)
    message(' completed!')
    return(safeObj)  
  }
}
