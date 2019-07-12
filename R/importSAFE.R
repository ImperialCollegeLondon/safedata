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
