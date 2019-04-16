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
  safeObj$startDate <- as.Date.numeric(df$Start.Date[1], origin="1899-12-30")
  safeObj$endDate <- as.Date.numeric(df$End.Date[1], origin="1899-12-30")
  
  return(safeObj)
}

processTaxa <- function (dataframe, safeObj) {
  #' Adds taxa information to SAFE data object
}

processLocations <- function (dataframe, safeObj) {
  #' Adds location information to SAFE data object
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