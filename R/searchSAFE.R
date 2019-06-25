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
}

apiSearch <- function (api, ids = NULL) {
  #' Conduct API search using the constructed api url
  #' 
  #' Wrapper function to reduce code repetition
  #' 
  #' @param api
  #' @param ids
  #' @return
  
  searchResult <- jsonlite::fromJSON(RCurl::getURL(api))
  checkApiCall(searchResult)
  if (!is.null(ids)) {
    if (!is.numeric(ids)) {
      stop(paste0('List of supplied reference IDs must be of type "numeric", ',
                  'got ', class(ids)))
    } else {
      return(intersect(searchResult$entries$zenodo_record_id, ids))
    }
  } else {
    return(searchResult$entries$zenodo_record_id)
  }
}

searchDates <- function (dates, matchType = 'intersect', ids = NULL, mostRecent = FALSE) {
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
  #' @param ids Numeric that restricts the returne API search to a subset of
  #'   Zenodo record IDs given in the \code{ids} parameter. This is typically
  #'   used to refine previous searches.
  #' @param mostRecent Logical indicating whether to restrict the API to
  #'   returning only the most recent versions of the datasets found. By default
  #'   all versions of the discovered projects are returned.  #' 
  #' @return An array of Zenodo record IDs for the SAFE project datasets found
  #'   to match the search criteria.
  #' @note This function returns Zenodo record IDs. On the Zenodo database each
  #'   version of a SAFE project has a unique record ID. Different versions of
  #'   the same dataset are grouped together under a single concept ID. See
  #'   \url{https://help.zenodo.org/} for more information on Zenodo versioning.
  #' @seealso \code{\link{getSafe}} for downloading SAFE datasets.
  
  if (!matchType %in% c('intersect', 'contain', 'within')) {
    stop(paste0('Parameter "matchType" must be one of "intersect", "contain", ',
                'or "within". Value passed (', matchType, ') is invalid'))
  }
  
  # check dates passed and set up API
  if (length(strsplit(dates, ','))[[1]] == 1) {
    api <- paste0('https://www.safeproject.net/api/search/dates?&date=',
                  gsub(' ', '', dates, fixed = TRUE))
  } else if (length(strsplit(dates, ','))[[1]] == 2) {
    api <- paste0('https://www.safeproject.net/api/search/dates?&date=',
                  gsub(' ', '', dates, fixed = TRUE), '&match_type=', matchType)
  } else {
    stop(paste0('Please pass either a single date or (comma-separated) date ',
                'range for searching. Current input (', dates, ') is invalid'))
  }
  
  if (mostRecent) {
    api <- paste0(api, '&most_recent=')
  }
  
  # run API search
  return(apiSearch(api))
}

searchFields <- function (fieldText = NULL, fieldType = NULL, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by data field information
  #' 
  #' Some more info
  #' 
  #' @param fieldText Character string to search for within the data field name
  #'   and description.
  #' @param fieldType Character string to search for within the field type (for
  #'   example field type \code(numeric)).
  #' @param ids Numeric that restricts the returne API search to a subset of
  #'   Zenodo record IDs given in the \code{ids} parameter. This is typically
  #'   used to refine previous searches.
  #' @param mostRecent Logical indicating whether to restrict the API to
  #'   returning only the most recent versions of the datasets found. By default
  #'   all versions of the discovered projects are returned.  #' 
  #' @return An array of Zenodo record IDs for the SAFE project datasets found
  #'   to match the search criteria.
  #' @note This function returns Zenodo record IDs. On the Zenodo database each
  #'   version of a SAFE project has a unique record ID. Different versions of
  #'   the same dataset are grouped together under a single concept ID. See
  #'   \url{https://help.zenodo.org/} for more information on Zenodo versioning.
  #' @seealso \code{\link{getSafe}} for downloading SAFE datasets.
  
  # check inputs
  if (is.null(fieldText) & is.null(fieldType)) {
    stop(paste0('Neither fieldText or fieldType inputs have been provided. ',
                'Please specify at least one!'))
  }
  if (!is.character(fieldText) | !is.character(fieldType)) {
    stop(paste0('"fieldText" and "fieldType" inputs must be of type character, ',
                'got (respectively) ', class(fieldText), ' and ', class(fieldType)))
  }
  
  # set up the API
  if (!is.null(fieldText) & !is.null(fieldType)) {
    api <- sprintf('https://www.safeproject.net/api/search/fields?text=%s&ftype=%s',
      fieldText, fieldType)
  } else if (!is.null(fieldText) & is.null(fieldType)) {
    api <- paste0('https://www.safeproject.net/api/search/fields?text=', fieldText)
  } else if (is.null(fieldText) & !is.null(fieldType)) {
    api <- paste0('https://www.safeproject.net/api/search/fields?ftype=', fieldType)
  }
  
  if (mostRecent) {
    api <- paste0(api, '&most_recent=')
  }
  
  # run API search
  return(apiSearch(api))
}

searchAuthor <- function (author, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by author
  #' 
  #' Some more info
  #' 
  #' @param author A character string used to search for datasets by author's 
  #'   full (or partial) names.
  #' @param ids Numeric that restricts the returne API search to a subset of
  #'   Zenodo record IDs given in the \code{ids} parameter. This is typically
  #'   used to refine previous searches.
  #' @param mostRecent Logical indicating whether to restrict the API to
  #'   returning only the most recent versions of the datasets found. By default
  #'   all versions of the discovered projects are returned.  #' 
  #' @return An array of Zenodo record IDs for the SAFE project datasets found
  #'   to match the search criteria.
  #' @note This function returns Zenodo record IDs. On the Zenodo database each
  #'   version of a SAFE project has a unique record ID. Different versions of
  #'   the same dataset are grouped together under a single concept ID. See
  #'   \url{https://help.zenodo.org/} for more information on Zenodo versioning.
  #' @seealso \code{\link{getSafe}} for downloading SAFE datasets.
  
  # check author input and set up API
  if (!is.character(author)) {
    stop(paste0('Parameter "author" should be of type char, got ', class(author)))
  } else {
    api <- paste0('https://www.safeproject.net/api/search/authors?&name=', author)
  }
  
  # set up the root API
  if (mostRecent) {
    api <- paste0(api, '&most_recent=')
  }
  
  # run API search
  return(apiSearch(api))
}

searchTaxa <- function (searchType, searchVal, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by taxa
  #' 
  #' Some more info
  #' 
  #' @param searchType Character that specifies taxa search criteria, which
  #'   should be one of "name" (taxa name), "gbif_id" (ID number on the GBIF
  #'   database, \url{https://www.gbif.org/en/}), or "rank" (species, family, etc).
  #' @param searchVal The value of parameter \code{searchType} to search against.
  #' @param ids Numeric that restricts the returne API search to a subset of
  #'   Zenodo record IDs given in the \code{ids} parameter. This is typically
  #'   used to refine previous searches.
  #' @param mostRecent Logical indicating whether to restrict the API to
  #'   returning only the most recent versions of the datasets found. By default
  #'   all versions of the discovered projects are returned.  #' 
  #' @return An array of Zenodo record IDs for the SAFE project datasets found
  #'   to match the search criteria.
  #' @note This function returns Zenodo record IDs. On the Zenodo database each
  #'   version of a SAFE project has a unique record ID. Different versions of
  #'   the same dataset are grouped together under a single concept ID. See
  #'   \url{https://help.zenodo.org/} for more information on Zenodo versioning.
  #' @seealso \code{\link{getSafe}} for downloading SAFE datasets,
  #'   \url{https://www.gbif.org/en/} for information on the GBIF database.
  #' @examples 
  #'   
  
  # check inputs and set up API
  if (!searchType %in% c('name', 'gbif_id', 'rank')) {
    stop(paste0('"searchType" must be one of "name", "gbif_id", or "rank", ',
                'got ', searchType))
  }
  api <- sprintf(
    'https://www.safeproject.net/api/search/taxa?%s=%s', searchType, searchVal)
  if (mostRecent) {
    api <- paste0(api, '&most_recent=')
  }
  
  # run API search
  return(apiSearch(api))
}

searchText <- function (text, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by free text search
  #' 
  #' Some more info
  #' 
  #' @param text Character string to look for within a SAFE dataset, worksheet,
  #'   title, field description, and dataset keywords.
  #' @param ids Numeric that restricts the returne API search to a subset of
  #'   Zenodo record IDs given in the \code{ids} parameter. This is typically
  #'   used to refine previous searches.
  #' @param mostRecent Logical indicating whether to restrict the API to
  #'   returning only the most recent versions of the datasets found. By default
  #'   all versions of the discovered projects are returned.  #' 
  #' @return An array of Zenodo record IDs for the SAFE project datasets found
  #'   to match the search criteria.
  #' @note This function returns Zenodo record IDs. On the Zenodo database each
  #'   version of a SAFE project has a unique record ID. Different versions of
  #'   the same dataset are grouped together under a single concept ID. See
  #'   \url{https://help.zenodo.org/} for more information on Zenodo versioning.
  #' @seealso \code{\link{getSafe}} for downloading SAFE datasets.
  #' @examples 
  #'   
  
  # check input and set up API
  if (!is.character(text)) {
    stop(paste0('"text" must be one of type character, got', class(text)))
  }
  api <- paste0('https://www.safeproject.net/api/search/text?text=', text)
  if (mostRecent) {
    api <- paste0(api, '&most_recent=')
  }
  
  # run API search
  return(apiSearch(api))
}

searchSpatial <- function (wkt = NULL, location = NULL, distance = NULL, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by free text search
  #' 
  #' Some more info
  #' 
  #' @param wkt Character string specifying a well-known text geometry. This is
  #'   assumed to use latitude and longitude in WGS84 (EPSG:4326), more
  #'   information on which can be read at
  #'   \url{https://en.wikipedia.org/wiki/World_Geodetic_System}.
  #' @param location Character string giving a location name used to select
  #'   query geometry from the SAFE gazetteer, more information on which can be
  #'   found at \url{https://www.safeproject.net/info/gazetteer}.
  #' @param distance Numeric search distance, specified in metres. All geometries
  #'   are converted to UTM 50N projection to provide appropriate distance
  #'   searching.
  #' @param ids Numeric that restricts the returne API search to a subset of
  #'   Zenodo record IDs given in the \code{ids} parameter. This is typically
  #'   used to refine previous searches.
  #' @param mostRecent Logical indicating whether to restrict the API to
  #'   returning only the most recent versions of the datasets found. By default
  #'   all versions of the discovered projects are returned.  #' 
  #' @return An array of Zenodo record IDs for the SAFE project datasets found
  #'   to match the search criteria.
  #' @note This function returns Zenodo record IDs. On the Zenodo database each
  #'   version of a SAFE project has a unique record ID. Different versions of
  #'   the same dataset are grouped together under a single concept ID. See
  #'   \url{https://help.zenodo.org/} for more information on Zenodo versioning.
  #' @seealso \code{\link{getSafe}} for downloading SAFE datasets,
  #'   \url{https://www.safeproject.net/info/gazetteer} for an index of defined
  #'   SAFE locations.
  #' @examples 
  #'   
  
  # check input and set up API
  if (!is.character(text)) {
    stop(paste0('"text" must be one of type character, got', class(text)))
  }
  api <- paste0('https://www.safeproject.net/api/search/text?text=', text)
  if (mostRecent) {
    api <- paste0(api, '&most_recent=')
  }
  
  # run API search
  return(apiSearch(api))
}