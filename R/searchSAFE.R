checkApiCall <- function (apiOutput) {
  #' Check a SAFE search API call for errors.
  #' 
  #' \code{checkApiCall} is a simple utility function used to check calls to the
  #' Zenodo SAFE database search API for invalid results. \code{checkApiCall}
  #' checks for a 404 message on the search output and prints the returned error
  #' message for user information.
  #' 
  #' @param apiOutput The output of a call to the Zenodo SAFE database search
  #'   API. This should be a \code{list} object.
  #' @note This function is for background use only, e.g. in functions such as
  #'   \code{\link{searchDates}}, \code{\link{searchTaxa}}, to verify API calls.
  
  if (is.null(apiOutput$count)) {
    if (apiOutput$error == 404) {
      stop('Invalid Zenodo API search: ', apiOutput$message, ' Please check all ',
           'terms.', call. = FALSE)
    }
  }
}

apiSearch <- function (api, ids = NULL) {
  #' Run an API call using the constructed \code{api} URL.
  #' 
  #' \code{apiSearch} is a wrapper function that downloads a URL (in this case
  #' assumed to be a Zenodo SAFE database search API construct) and converts the
  #' resulting JSON data into a readable R object (a \code{list}). An array of
  #' Zenodo record IDs (matching the Zenodo search API results) are extracted
  #' and compared against a supplied set of (optional) \code{ids}, enabling the
  #' user to restrict the search outcome to a particular set of record IDs.
  #' 
  #' @param api The constructed Zenodo SAFE database search API.
  #' @param ids A set of (numeric) Zenodo record IDs that restricts the returned
  #'   search values. Only record IDs identified in the search API that appear
  #'   in the \code{ids} subset are returned to the user.
  #' @return A set of numeric Zenodo record IDs for SAFE datasets found to match
  #'   the search criteria specified in the \code{api} construct.
  #' @note It is assumed that the \code{api} URL passed is of the correct format
  #'   to perform a search of the Zenodo SAFE database.
  #' @seealso \code{\link{searchDates}}, \code{\link{searchFields}},
  #'   \code{\link{searchAuthor}}, \code{\link{searchTaxa}},
  #'   \code{\link{searchText}}, \code{\link{searchSpatial}} for building
  #'   different search query types, \url{https://www.safeproject.net/api/} for
  #'   a full outline of the Zenodo search API used to interface with SAFE
  #'   projects.
  
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
  #' Search SAFE projects by date.
  #' 
  #' Search for SAFE datasets by temporal extent. \code{searchDates} allows
  #' users to search the Zenodo database for SAFE projects conducted on a
  #' specific date, or over a given date range.
  #' 
  #' @param dates A character string containing one or two (comma separated) 
  #'   dates in ISO format (yyyy-mm-dd).
  #' @param matchType  A character string - one of 'intercept', 'contain', or 
  #'   'within' used to match the provided dates to the temporal extents of SAFE
  #'   datasets. Defaults to 'intersect'. The 'contain' option returns datasets
  #'   that span a date range, while 'within' returns datasets that fall within
  #'   the given range. Note that this parameter is only utilised when a date
  #'   range is specified (i.e. it is not used for single-date searches).
  #' @param ids Numeric that restricts the returned API search to a subset of
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
  #'   searchDates('2014-06-12')
  #'   searchDates('2014-06-12,2015-06-11')
  #'   searchDates('2014-06-12,2015-06-11', matchType = 'contain')
  #'   searchDates('2014-06-12,2015-06-11', matchType = 'within')
  #'   searchDates('2014-06-12,2015-06-11', mostRecent = TRUE)
  #'   searchDates('2014-06-12,2015-06-11', 
  #'               ids = c(3247465, 3081059, 1478526, 3251902))
  
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
  return(apiSearch(api, ids))
}

searchFields <- function (fieldText = NULL, fieldType = NULL, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by data field information.
  #' 
  #' Search for SAFE datasets by data field information. \code{searchFields}
  #' allows users to search the Zenodo database for SAFE projects recording
  #' specific data types, for example temperature data, or numeric data types.
  #' 
  #' @param fieldText Character string to search for within the data field name
  #'   and description.
  #' @param fieldType Character string to search for within the field type (for
  #'   example field type \code{numeric}).
  #' @param ids Numeric that restricts the returned API search to a subset of
  #'   Zenodo record IDs given in the \code{ids} parameter. This is typically
  #'   used to refine previous searches.
  #' @param mostRecent Logical indicating whether to restrict the API to
  #'   returning only the most recent versions of the datasets found. By default
  #'   all versions of the discovered projects are returned.
  #' @return An array of Zenodo record IDs for the SAFE project datasets found
  #'   to match the search criteria.
  #' @note This function returns Zenodo record IDs. On the Zenodo database each
  #'   version of a SAFE project has a unique record ID. Different versions of
  #'   the same dataset are grouped together under a single concept ID. See
  #'   \url{https://help.zenodo.org/} for more information on Zenodo versioning.
  #' @seealso \code{\link{getSafe}} for downloading SAFE datasets,
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/#field-types}
  #'   for accepted SAFE field types.
  #' @examples
  #'   searchFields(fieldText = 'temperature')
  #'   searchFields(fieldType = 'numeric')
  #'   searchFields(fieldText = 'temperature', fieldType = 'numeric')
  #'   searchFields(fieldText = 'temperature', mostRecent = TRUE)
  #'   searchFields(fieldText = 'temperature', ids = c(3238498, 3247485, 3251900))
  
  # check inputs
  if (is.null(fieldText) & is.null(fieldType)) {
    stop(paste0('Neither "fieldText" or "fieldType" have been provided. Please ',
                'specify at least one!'))
  }
  if (!is.null(fieldText)) {
    if (!is.character(fieldText)) {
      stop(paste0('"fieldText" must be of type character, got ', class(fieldText)))
    }
  }
  if (!is.null(fieldType)) {
    if (!is.character(fieldType)) {
      stop(paste0('"fieldType" must be of type character, got ', class(fieldType)))
    }
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
  return(apiSearch(api, ids))
}

searchAuthor <- function (author, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by author name.
  #' 
  #' Search for SAFE datasets by author name. \code{searchAuthor} enables users
  #' to search the Zenodo database for SAFE projects containing the given
  #' \code{author} among the author list. Partial author names may also be used.
  #' Note that this function is not case sensitive.
  #' 
  #' @param author A character string used to search for datasets by author's 
  #'   full (or partial) names.
  #' @param ids Numeric that restricts the returned API search to a subset of
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
  #'   searchAuthor('Ewers')
  #'   searchAuthor('Ew')
  #'   searchAuthor('Ewers', mostRecent = TRUE)
  #'   searchAuthor('Ewers', ids = c(1303014, 1237725, 3081059, 1323504, 3251902))
  
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
  return(apiSearch(api, ids))
}

searchTaxa <- function (searchType, searchVal, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by taxa.
  #' 
  #' Search for SAFE datasets by taxon information. \code{searchTaxa} allows
  #' users to search the Zenodo database for SAFE projects containing data
  #' pertaining to particular taxa. Taxa may be defined using one of three
  #' \code{searchType} identifiers: name, gbif_id, or rank.
  #' 
  #' @param searchType Character that specifies taxa search criteria, which
  #'   should be one of "name" (taxa name), "gbif_id" (ID number on the GBIF
  #'   database, \url{https://www.gbif.org/en/}), or "rank" (species, family, etc).
  #' @param searchVal The value of parameter \code{searchType} to search against.
  #' @param ids Numeric that restricts the returned API search to a subset of
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
  #'   searchTaxa(searchType = 'name', searchVal = 'Formicidae')
  #'   searchTaxa(searchType = 'gbif_id', searchVal = 4342)
  #'   searchTaxa(searchType = 'rank', searchVal = 'Family')
  #'   searchTaxa(searchType = 'name', searchVal = 'Formicidae', mostRecent = TRUE)
  #'   searchTaxa(searchType = 'name', searchVal = 'Formicidae', 
  #'              ids = c(1198302, 1198472, 1237732, 1198839, 1303014, 1237725))
  
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
  return(apiSearch(api, ids))
}

searchText <- function (text, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by free text search.
  #' 
  #' Search for SAFE datasets using a free text search. \code{searchText} allows
  #' users to perform a free text search on SAFE projects stored on the Zenodo
  #' database. This function will search for values matching \code{text} within
  #' dataset, worksheet, and data field descriptions, as well as in titles and
  #' dataset keywords.
  #' 
  #' @param text Character string to look for within a SAFE dataset, worksheet,
  #'   title, field description, and dataset keywords.
  #' @param ids Numeric that restricts the returned API search to a subset of
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
  #'   searchText('forest')
  #'   searchText('ant')
  #'   searchText('forest', mostRecent = TRUE)
  #'   searchText('forest', ids = searchText('ant'), mostRecent = TRUE)
  
  # check input and set up API
  if (!is.character(text)) {
    stop(paste0('"text" must be one of type character, got', class(text)))
  }
  api <- paste0('https://www.safeproject.net/api/search/text?text=', text)
  if (mostRecent) {
    api <- paste0(api, '&most_recent=')
  }
  
  # run API search
  return(apiSearch(api, ids))
}

searchSpatial <- function (wkt = NULL, location = NULL, distance = NULL, ids = NULL, mostRecent = FALSE) {
  #' Search SAFE projects by spatial sampling area/named location.
  #' 
  #' Search for SAFE datasets on the Zenodo database using either a 
  #' user-provided geometry or the geometry of a named location from the SAFE
  #' gazetteer (\url{https://www.safeproject.net/info/gazetteer}). The sampling
  #' locations provided in each SAFE dataset are tested to see if they intersect
  #' the search geometry. A buffer \code{distance} can aso be provided to search
  #' around the query geometry.
  #' 
  #' \code{searchSpatial} will not retrieve datasets that have not provided
  #' sampling locations or use newly defined locations that are missing
  #' coordinate information. When searching by bounding box (geometric shape)
  #' the dataset geographic extent - which is provided for all datasets - is used.
  #' 
  #' Note that only one of \code{wkt} (well-known text geometry) or
  #' \code{location} (named location on the SAFE gazetteer) should be provided.
  #' 
  #' @param wkt Character string specifying a well-known text geometry. This is
  #'   assumed to use latitude and longitude in WGS84 (EPSG:4326), more
  #'   information on which can be read at
  #'   \url{https://en.wikipedia.org/wiki/World_Geodetic_System}. Useful
  #'   guidelines on constructing well-known text representations of geometry
  #'   can be found at
  #'   \url{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}.
  #' @param location Character string giving a location name used to select
  #'   query geometry from the SAFE gazetteer, more information on which can be
  #'   found at \url{https://www.safeproject.net/info/gazetteer}.
  #' @param distance Numeric search distance, specified in metres. All geometries
  #'   are converted to UTM 50N projection to provide appropriate distance
  #'   searching.
  #' @param ids Numeric that restricts the returned API search to a subset of
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
  #'   The function will not retrieve datasets for which no location information
  #'   is provided.
  #' @seealso \code{\link{getSafe}} for downloading SAFE datasets,
  #'   \url{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}
  #'   for setting up well-known text geometries,
  #'   \url{https://www.safeproject.net/info/gazetteer} for an index of defined
  #'   SAFE locations.
  #' @examples 
  #'   searchSpatial(wkt = 'Point(116.5 4.75)')
  #'   searchSpatial(wkt = 'Point(116.5 4.75)', distance = 100000)
  #'   searchSpatial(wkt = 'Polygon((110 0, 110 10,120 10,120 0,110 0))')
  #'   searchSpatial(location = 'A_1')
  #'   searchSpatial(location = 'A_1', distance = 2500)
  #'   searchSpatial(location = 'A_1', mostRecent = TRUE)
  #'   searchSpatial(location = 'A_1', distance = 2500,
  #'                 ids = c(1303018, 1237730, 1198585, 1228188, 1198461, 119832))
  
  # check inputs and set up API
  api <- 'https://www.safeproject.net/api/search/spatial?'
  if (is.null(wkt) & is.null(location)) {
    stop(paste0('Neither "wkt" or "location" have been provided! Please ',
                'specify at least one.'))
  } else if (!is.null(wkt) & !is.null(location)) {
    stop(paste0('Both "wkt" and "location" parameters have been provided! ',
                'Only one should be specified.'))
  } else {
    if (!is.null(wkt)) {
      if (grepl('^Point|^LineString|^Polygon', wkt)) {
        api <- paste0(api, 'wkt=', gsub(' ', '%20', wkt))
      } else {
        stop(paste0('"wkt" must be a well-known text geometry representation ',
                    'defined using one of "Point", "LineString", or "Polygon".',
                    ' Got ', wkt))
      }
    } else if (!is.null(location)) {
      if (!is.character(location)) {
        stop(paste0('"location" should be of type character, got ', 
                    class(location)))
      } else {
        api <- paste0(api, 'location=', location)
      }
    }
  }
  
  if (!is.null(distance)) {
    if (!is.numeric(distance)) {
      stop(paste0('"distance" should be of type numeric, got ', class(distance)))
    } else if (distance%%1 != 0) {
      stop(paste0('"distance" should be an integer, got ', distance))
    } else {
      api <- paste0(api, '&distance=', sprintf('%i', distance))
    }
  }
  
  if (mostRecent) {
    api <- paste0(api, '&most_recent=')
  }
  
  # run API search
  return(apiSearch(api, ids))
}