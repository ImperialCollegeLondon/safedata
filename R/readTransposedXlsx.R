readTransposedXlsx <- function (path, sheetName) {
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
  dfT <- as.data.frame(t(df[-1]), stringsAsFactors = FALSE)
  names(dfT) <- t(df[,1])
  dfT <- as.data.frame(lapply(dfT,type.convert))
  return(dfT)
}