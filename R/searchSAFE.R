#' searchSafe <- function (most_recent = FALSE, ids = NULL, files = FALSE, 
#'                         record = NULL, taxa = NULL, dates = NULL, matchType = 'intersect', fieldText = NULL, fieldType = NULL,
#'                         author = NULL, text = NULL, spatial = NULL, 
#'                         locations = NULL, bbox = NULL) {
#'   #' Function to search the SAFE Zenodo database using API keywords
#'   #' 
#'   #' Here's a bit more info about what this function does and the arguments it
#'   #' accepts. Various search terms to explore datasets, various endpoints to
#'   #' specify certain search conditions.
#'   #' 
#'   #' @param most_recent 
#'   #' @param ids  
#'   #' @param files Logical which if \code{TRUE} returns a dataframe of filenames
#'   #'   associated with the discovered Record IDs. There will usually be only a
#'   #'   single Excel file associated with a given record version, but external
#'   #'   data files will also be listed.
#'   #' @param record Record ID which, if provided, returns the metadata for the
#'   #'   version. This is equivalent to the output of a call to
#'   #'   \code{\link{zenodoRecordApiLookup}}
#'   #' @param taxa
#' 
#'   #' @param fieldText Search for datasets by data field information.
#'   #' @param fieldType Search for datasets by data field information.
#'   #' @param author A character string used to search for datasets by author's 
#'   #'   full (or partial) names.
#'   #' @param text Search datasets using a free-text search. The character string
#'   #'   \code{text} is searched for within datasets, worksheets, titles, keywords,
#'   #'   and data field descriptions.
#'   #' @param spatial
#'   #' @param locations
#'   #' @param bbox
#'   #' 
#'   #' @return 
#'   #'   
#'   #' @seealso \url{https://safeproject.net/api} for a detailed walkthrough on
#'   #'   using the SAFE search API tool
#'   #' @export
#' 
#'   api <- 
#'   
#'   if (files) {
#'      # do something not related to search terms
#'   } else {
#'     # process search terms
#'     api <- paste0(api, 'search/')
#'     
#'     
#'     # FIELD TYPES
#'     
#'     # AUTHOR
#'     if (!is.null(author)) {
#'       if (!is.character(author)) {
#'         stop(paste0('Parameter "author" should be of type char, received',
#'                     class(author)))
#'       } else {
#'         api <- paste0(api, 'authors?name=', author)
#'       }
#'     }
#'     
#'   }
#'   print(api)
#'   
#'   return(searchResult)
#'   #return(searchResult$entries$zenodo_record_id)
#' }

checkApiCall <- function (apiOutput) {
  #' Check an API call for errors
  #' 
  #' Some more info
  #' 
  #' @param apiOutput
  
  if (is.null(apiOutput$count)) {
    if (apiOutput$error == 404) {
      stop('Invalid Zenodo API search: please check all terms.', call. = FALSE)
    }
  }
  if (apiOutput$count == 0) {
    warning('No results returned from search!')
  }
}

searchDates <- function (dates, matchType = 'intersect', mostRecent = FALSE, 
                         ids = NULL, within = NULL) {
  #' Search SAFE projects by dates
  #' 
  #' Some more info
  #' 
  #' @param dates Search for datasets by temporal extent. A character string 
  #'   containing one or two (comma separated) dates in ISO format (yyyy-mm-dd).
  #' @param matchType  A character string - one of 'intercept', 'contain', or 
  #'   'within' used to match the provided dates to the temporal extents of SAFE
  #'   datasets. Defaults to 'intersect'. The 'contain' option returns datasets
  #'   that span a date range while 'within' returns datasets that fall within
  #'   the given range. Note that this parameter is only utilised when a date
  #'   range is specified (i.e. it is not used for single-date searches).
  #' @param mostRecent Logical indicating whether to restrict the API to
  #'   returning only the most recent versions of the datasets found. By default
  #'   all versions of the discovered projects are returned.
  #' @param ids Logical enabling the user to restrict the API search to a subset
  #'   of Zenodo record IDs. This is typically used to refine previous searches.
  #' @param within
  #' 
  #' @return On the Zenodo database each version of a SAFE project submission
  #'   has a unique record ID. Different versions of the same SAFE dataset are
  #'   grouped together under a single concept ID. See \url{https://help.zenodo.org/} for more information on Zenodo versioning.
  
  if (!matchType %in% c('intersect', 'contain', 'within')) {
    stop(paste0('Parameter "matchType" must be one of "intersect", "contain", ',
                'or "within". Value passed (', matchType, ') is invalid'))
  }
  
  if (mostRecent) {
    api <- 'https://www.safeproject.net/api/search/dates?most_recent=&date='
  } else if (!is.null(ids)) {
    api <- 'https://www.safeproject.net/api/search/dates?'
    for (id in ids) {
      api <- paste0(api, 'ids=', id, '&')
    }
    api <- paste0(api, 'date=')
  } else {
    api <- 'https://www.safeproject.net/api/search/dates?&date='
  }
  
  if (length(strsplit(dates, ','))[[1]] == 1) {
    api <- paste0(api, gsub(' ', '', dates, fixed = TRUE))
  } else if (length(strsplit(dates, ','))[[1]] == 2) {
    api <- paste0(api, gsub(' ', '', dates, fixed = TRUE), '&match_type=', matchType)
  } else {
    stop(paste0('Please pass either a single date or (comma-separated) date ',
                'range for searching. Current input (', dates, ') is invalid'))
  }
  
  searchResult <- jsonlite::fromJSON(RCurl::getURL(api))
  checkApiCall(searchResult)
  return(searchResult$entries$zenodo_record_id)
}