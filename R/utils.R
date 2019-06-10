nullToNa <- function(x) {
  #' Convert \code{NULL} values to \code{NA}
  
  x[is.null(x)] <- NA
  return(x)
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

setSafeDir <- function (dir = NULL) {
  #' Set the local SAFE data directory
  #'
  #' Sets the local SAFE data directory using R "options". If an invalid/no
  #' directory is specified, defaults to the current working directory.
  #'
  #' @param dir, the directory to set as the SAFE_data_dir (defaults to 
  #'   \code{getwd()})
  
  if (is.null(dir)) {
    warning(paste0('SAFE_data_dir not supplied, ', 
                   'defaulting to current working directory'))
    dir = getwd()
  } else if (!dir.exists(dir)) {
    warning(paste0('Invalid SAFE_data_dir supplied, ', 
                   'defaulting to current working directory'))
    dir = getwd()
  }
  options('SAFE_data_dir' = dir)
}