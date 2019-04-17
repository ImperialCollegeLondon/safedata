readTransposedXlsx <- function (file, sheetName) {
  #' Read a transposed .xlsx file into a dataframe
  #' 
  #' Provides capability to read .xlsx files that are transposed (i.e. 
  #' organised with headers in rows and data across columns) into a standard R
  #' dataframe. This function preserves data types.
  #' 
  #' @param path The path to the .xlsx file to be opened
  #' @param sheetName The name of the worksheet to be imported
  #' 
  #' @note Dependency on package ReadXL
  
  df <- read_xlsx(file, sheetName, col_names=FALSE)
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
  safeObj$dataSheets <- gsub(" ", "", lapply(subset(
    as.character(df$Worksheet.name), !is.na(df$Worksheet.name)), simpleCap))
  safeObj$startDate <- as.Date.numeric(df$Start.Date[1], origin="1899-12-30")
  safeObj$endDate <- as.Date.numeric(df$End.Date[1], origin="1899-12-30")
  
  return(safeObj)
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
  
  
}

safeWrapper <- function (file) {
  #' Wrapper to open and process SAFE data file
}

printSummary <- function (safeObj) {
  #' print summary information for SAFE data object
}