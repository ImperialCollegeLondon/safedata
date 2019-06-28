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

setSafeDir <- function(dir, init=FALSE) {
    #' Set the local SAFE data directory
    #'
    #' Validate and sets the local SAFE data directory in options('safedata.dir').
	#' The named directory must contain the index file ('safe_data_index.rds'),
	#' although this will be created if \code{init} is set to true.
    #'
    #' @param dir A path to the directory to set as the SAFE data directory.
	#' @param init A boolean to indicate whether to initialise the directory
	#'     as a SAFE data directory.
	#' @export

	if (! dir.exists(dir)) {
        stop("Directory not found.")
    } 
	
	index_found <- file.exists(file.path(dir, 'safe_data_index.rds'))
	
	if(create & index_found){
		warning("Index already exists, setting SAFE data directory,.")
		options(safedata.dir = dir)		
	} else if(create & ! index_found){
		message('Downloading SAFE dataset index.')
		index <- jsonlite::fromJSON('https://www.safeproject.net/api/files')
		saveRDS(index, file.path(dir, 'safe_data_index.rds'))
		options(safedata.dir = dir)
	} else if(! index_found){
		stop("Index not found, not a SAFE data directory.")
	} else {
		options(safedata.dir = dir)
	}	
}

readTransposedXlsx <- function (path, sheetName, ...) {
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
    readxl::read_xlsx(path, sheet = sheetName, col_names = FALSE, ...))
  dfT <- as.data.frame(t(df[-1]), stringsAsFactors = FALSE)
  names(dfT) <- t(df[,1])
  dfT <- as.data.frame(lapply(dfT, utils::type.convert))
  
  return(dfT)
}
