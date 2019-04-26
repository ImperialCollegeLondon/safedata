readTransposedXlsx <- function (file, sheetName, ...) {
  #' Read a transposed .xlsx file into a dataframe
  #' 
  #' Provides capability to read .xlsx files that are transposed (i.e. 
  #' organised with headers in rows and data across columns) into a standard R
  #' dataframe. This function preserves data types.
  #' 
  #' @param path The path to the .xlsx file to be opened
  #' @param sheetName The name of the worksheet to be imported
  #' @param ... Optional arguments to be passed to read_xlsx()
  #' 
  #' @note Dependency on package ReadXL
  
  df <- read_xlsx(file, sheetName, col_names=FALSE, ...)
  dfT <- as.data.frame(t(df[-1]), stringsAsFactors=FALSE)
  names(dfT) <- t(df[,1])
  dfT <- as.data.frame(lapply(dfT, type.convert))
  
  return(dfT)
}

simpleCap <- function (str) {
  #' Capitalize the first letter of each word in a string
  
  x <- strsplit(str, " ")[[1]]
  y <- paste(toupper(substring(x, 1, 1)), substring(x, 2), sep="", collapse=" ")
  return(y)
}

processSummary <- function (df) {
  #' Process summary "meta" information for SAFE project data
  #' 
  #' Reads and assigns variables from the provided dataframe, which must be a
  #' transposed "Summary" sheet from a SAFE project data submission.
  #' 
  #' @param dataframe Dataframe containing summary information
  #' @return List object containing summary information
  
  safeObj <- list()
  safeObj$projectID <- df$SAFE.Project.ID[1]
  safeObj$title <- as.character(df$Title[1])
  safeObj$workSheets <- gsub(" ", "", lapply(subset(
    as.character(df$Worksheet.name), !is.na(df$Worksheet.name)), simpleCap))
  for (i in 1:length(safeObj$workSheets)) {
    safeObj[[safeObj$workSheets[i]]] <- list()
  }
  safeObj$startDate <- as.Date.numeric(df$Start.Date[1], origin="1899-12-30")
  safeObj$endDate <- as.Date.numeric(df$End.Date[1], origin="1899-12-30")
  
  return(safeObj)
}

printSummary <- function (safeObj) {
  #' Prints summary information for SAFE project data
  
  
}

processTaxa <- function (file, safeObj) {
  #' Adds taxa information to SAFE data object

  safeObj$Taxa <- as.data.frame(read_xlsx(file, "Taxa", col_names=TRUE))
  return(safeObj)
}

processLocations <- function (file, safeObj) {
  #' Adds location information to SAFE data object
  
  safeObj$Locations <- as.data.frame(read_xlsx(file, "Locations",
                                               col_names=TRUE))
  return(safeObj)
}

processData <- function (file, safeObj) {
  #' Adds data worksheets to SAFE data object
  
  sheets <- excel_sheets(file)
  sheetsNormed <- gsub(" ", "", lapply(sheets, simpleCap))
  
  for (i in 1:length(safeObj$workSheets)) {
    idx <- which.max(sheetsNormed==safeObj$workSheets[i])
    
    # get line index where data begins using field_name ID
    fullData <- as.data.frame(read_xlsx(file, sheets[idx], col_names=FALSE))
    firstDataRow <- which.max(fullData[,1] == "field_name")
    
    # extract meta information from header lines
    headerInfo <- readTransposedXlsx(fPath, sheets[idx], n_max=firstDataRow-1)
    fieldTypes <- c("numeric", sapply(headerInfo$field_type, getDataClass))
    safeObj[[safeObj$workSheets[i]]]$attributes <- headerInfo
    
    # store data (without header info) "en masse"
    data <- as.data.frame(read_xlsx(fPath, sheets[idx], col_names=TRUE,
                                    col_types=fieldTypes, skip=firstDataRow-1))

    # convert categorical data types
    categoricals <- c(FALSE, sapply(headerInfo$field_type, isCategorical))
    factorCols <- names(data)[categoricals]
    data[factorCols] <- lapply(data[factorCols], factor)
    safeObj[[safeObj$workSheets[i]]]$data <- data
  }
  
  return(safeObj)
}

getDataClass <- function (safeType) {
  #' Get the R data type from the SAFE field_type variable
  
  #' @param safeType The SAFE field_type variable
  #' @return The readxl data type
  #' @seealso \url{https://www.safeproject.net/dokuwiki/} for accepted SAFE data 
  #'   field types and \url{https://cran.r-project.org/web/packages/readxl/} for
  #'   ReadXl data types
  
  typeDate <- c("Date","Datetime", "Time")
  typeText <- c("Location", "ID", "Taxa", "Replicate", "Abundance")
  typeNum <- c("Latitude", "Longitude", "Numeric")
  typeFac <- c("Categorical", "Ordered Categorical", "Categorical Trait",
               "Numeric Trait", "Categorical Interaction", "Numeric Interaction")
  
  if (safeType %in% typeDate) {
    return("date")
  } else if (safeType %in% typeText) {
    return("text")
  } else if (safeType %in% typeNum) {
    return("numeric")
  } else if (safeType %in% typeFac) {
    return("text")
  } else {
    return("guess")
  }
}

isCategorical <- function (safeType) {
  #' Check if the SAFE data type is of R type "categorical"/"factor"
  
  if (safeType %in% c("Categorical", "Ordered Categorical", "Categorical Trait",
                      "Numeric Trait", "Categorical Interaction",
                      "Numeric Interaction")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

safeWrapper <- function (file) {
  #' Wrapper to open and process SAFE data file
  
  summary <- readTransposedXlsx(file, sheet="Summary")
  safeObj <- processSummary(summary)
  safeObj <- processTaxa(file, safeObj)
  safeObj <- processLocations(file, safeObj)
  safeObj <- processData(file, safeObj)
  
  return(safeObj)
}