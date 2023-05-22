#' SAFE dataset search functions.
#'
#' In addition to the datasets stored on Zenodo, the SAFE Project website
#' provides an API to search dataset metadata in more depth. The search
#' functions access this API and return \code{\link{safe_record_set}} objects
#' identifying datasets that match a particular query.
#'
#' The API provides endpoints to search datasets by date extents, data
#' worksheet fields, authors, taxa, free text and by spatial query. All
#' of the functions accept the argument \code{most_recent}, which restricts
#' the returned datasets to the most recent versions of each matching dataset
#' concept. The functions can also be passed an existing
#' \code{\link{safe_record_set}} object to search within the results
#' of a previous search.
#'
#' The \code{match_type} parameter specifies how to match date ranges and must
#' be one of "intersect" (default), "contain", or "within". The "contain" option
#' returns datasets that span a date range,  "within" returns datasets that
#' fall within the given range and "intersect" selects datasets that overlap any
#' part of the date range. Note that match_type is ignored when only a single
#' date is provided.
#'
#' @section Spatial searches:
#' For spatial searches, users can select a location name from a SAFE
#' data gazetteer (see e.g. \url{https://www.safeproject.net/info/gazetteer}
#' or \code{\link{load_gazetteer}}) or provide a WKT geometry. The sampling
#' locations provided in each SAFE dataset are tested to see if they intersect
#' the search geometry.
#'
#' A buffer \code{distance} can aso be provided to extend the search around the
#' query geometry. Note that although WKT geometries should be provided
#' using WGS84 lat/long coordinates, since this is typical field GPS data,
#' distances must be provided as metres and all proximity calculations take
#' place in the UTM50N projected coordinate system.
#'
#' The \code{search_spatial} function will not retrieve datasets that have not
#' provided sampling locations or use newly defined locations that are missing
#' coordinate information.
#'
#' @section Links:
#' \describe{
#'    \item{SAFE data API}{e.g. \url{https://www.safeproject.net/api}}
#'    \item{Worksheet field types}{\url{https://safedata-validator.readthedocs.io/en/latest/data_format/data.html#field-types}}
#'    \item{SAFE gazetteer}{See \code{\link{load_gazetteer}} and e.g.
#'          \url{https://www.safeproject.net/info/gazetteer}}
#'    \item{WKT}{\url{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}}
#' }
#' @param dates A vector of length 1 or 2, containing either ISO format date
#'    character strings ("yyyy-mm-dd") or \code{POSIXt} dates.
#' @param match_type  A character string (see Details).
#' @param field_text Text to search for within the data worksheet field name
#'    and description.
#' @param field_type A data worksheet field type (see Links).
#' @param author A character string used to search for datasets by author
#'    full (or partial) names.
#' @param taxon_name The scientific name of a taxon to search for.
#' @param taxon_rank A taxonomic rank to search for.
#' @param gbif_id A GBIF taxonomic ID number.
#' @param text Character string to look for within a SAFE dataset, worksheet,
#'    title, field description, and dataset keywords.
#' @param wkt A well-known text geometry string, assumed to use latitude and
#'    longitude in WGS84 (EPSG:4326).
#' @param location The name of a location in the SAFE gazetteer.
#' @param distance A buffer distance for spatial searches, giving the distance
#'    in metres within which to match either location or wkt searches.
#' @param ids A set of SAFE dataset record IDs to restrict a search. This will
#'    typically be a \code{\link{safe_record_set}} object returned by another
#'    search but can also be a vector of record ids in any of the formats
#'    accepted by \code{\link{validate_record_ids}}.
#' @param most_recent Logical indicating whether to restrict the API to
#'    returning only the most recent versions of the datasets found. By default
#'    all versions of matching dataset concepts are returned.
#' @return An object of class \code{\link{safe_record_set}} of datasets that
#'    match the query.
#' @examples
#' \donttest{
#' search_dates("2014-06-12")
#' search_dates(as.POSIXct(c("2014-06-12", "2015-06-11")))
#' search_dates(c("2014-06-12", "2015-06-11"), match_type = "contain")
#' search_fields(field_text = "temperature")
#' search_fields(field_type = "numeric")
#' search_fields(field_text = "temperature", field_type = "numeric")
#' search_authors("Ewers")
#' search_taxa(taxon_name = "Formicidae")
#' search_taxa(taxon_id = 4342, taxon_auth = "GBIF")
#' search_taxa(taxon_rank = "family")
#' search_text("forest")
#' search_text("ant")
#' search_spatial(wkt = "Point(116.5 4.75)")
#' search_spatial(wkt = "Point(116.5 4.75)", distance = 100000)
#' search_spatial(wkt = "Polygon((110 0, 110 10,120 10,120 0,110 0))")
#' search_spatial(location = "A_1")
#' search_spatial(location = "A_1", distance = 2500)
#'
#' # combining searches using logical operators
#' fish <- search_taxa("Actinopterygii")
#' odonates <- search_taxa("Odonata")
#' ewers <- search_authors("Ewers")
#' aquatic <- fish | odonates
#' aquatic_ewers <- aquatic & ewers
#' all_in_one <- (fish | odonates) & ewers
#' }
#' @name search_safe

NULL


search_dates <- function(dates, match_type = "intersect",
                         most_recent = FALSE, ids = NULL) {
    #' @describeIn search_safe Search datasets by date extent
    #' @export

    match_type <- match.arg(match_type, c("intersect", "contain", "within"))

    # check dates passed
    validate_query_param("dates", dates, c("character", "POSIXt"), c(1, 2))

    # Convert character to POSIXt to validate content
    if (inherits(dates, "character")) {
        dates <- try(as.POSIXct(dates))
        if (inherits(dates, "try-error")) {
            stop("dates not formatted in POSIX format (yyyy-mm-dd).")
        }
    }

    dates <- paste(format(sort(dates), "%Y-%m-%d"), collapse = ",")

    # pass the query string to the api handler
    params <- c(date = dates, match_type = match_type)
    return(safe_api_search("dates", params, ids, most_recent))
}


search_fields <- function(field_text = NULL, field_type = NULL,
                          ids = NULL, most_recent = FALSE) {
    #' @describeIn search_safe Search data worksheet field metadata.
    #' @export

    # check inputs
    validate_query_param("field_text", field_text)
    validate_query_param("field_type", field_type)

    params <- c(text = field_text, ftype = field_type)
    return(safe_api_search("fields", params, ids, most_recent))
}


search_authors <- function(author, ids = NULL, most_recent = FALSE) {
    #' @describeIn search_safe Search by dataset author
    #' @export

    # check inputs
    validate_query_param("author", author)

    params <- c(name = author)
    return(safe_api_search("authors", params, ids, most_recent))
}


search_taxa <- function(taxon_name = NULL, taxon_rank = NULL,
                        taxon_id = NULL, taxon_auth = c("GBIF", "NCBI"),
                        ids = NULL, most_recent = FALSE) {
    #' @describeIn search_safe Search by taxon name, rank or taxon ID.
    #' @export

    taxon_auth <- match.arg(taxon_auth)

    # check inputs
    validate_query_param("taxon_name", taxon_name)
    validate_query_param("taxon_rank", taxon_rank)
    validate_query_param("taxon_id", gbif_id, class = "numeric")
    validate_query_param("taxon_auth", taxon_auth)

    params <- c(
        name = taxon_name, rank = taxon_rank,
        taxon_id = taxon_id, auth = taxon_auth
    )
    return(safe_api_search("taxa", params, ids, most_recent))
}


search_text <- function(text, ids = NULL, most_recent = FALSE) {
    #' @describeIn search_safe Search dataset, worksheet and field titles
    #'    and descriptions
    #' @export

    validate_query_param("text", text)

    params <- c(text = text)
    return(safe_api_search("text", params, ids, most_recent))
}


search_spatial <- function(wkt = NULL, location = NULL, distance = NULL,
                           ids = NULL, most_recent = FALSE) {
    #' @describeIn search_safe Search by spatial sampling area/named location.
    #' @export

    # check inputs
    validate_query_param("wkt", wkt)
    validate_query_param("location", location)
    validate_query_param("distance", distance, "numeric")

    # look for one or other of wkt and locations and do further validation
    if (!xor(is.null(wkt), is.null(location))) {
        stop("Provide either wkt or location.")
    } else if (!is.null(wkt)) {
        ft <- try(sf::st_as_sfc(wkt))
        if (inherits(ft, "try-error")) {
            stop("wkt string not valid")
        }
        # TODO - check whether passing WGS84 or UTM50N
    } else {
        gazetteer <- load_gazetteer()
        if (!location %in% gazetteer$location) {
            stop("Location name not found in gazetteer")
        }
    }

    # make sure distance is not passed using scientific notation (to
    # nano metre accuracy)
    distance <- sprintf("%0.0f", distance)

    # pass the params to the handler
    params <- c(wkt = wkt, location = location, distance = distance)
    return(safe_api_search("spatial", params, ids, most_recent))
}


safe_api_search <- function(endpoint, params, ids = NULL,
                            most_recent = FALSE) {
    #' Internal SAFE dataset API search functions
    #'
    #' The two internal functions described here handle validating the
    #' parameters passed to the exported search functions and then constructing
    #' API calls from the parameters and the common \code{ids} and
    #' \code{most_recent} arguments.
    #'
    #' @param endpoint The name of the search API endpoint to be used.
    #' @param params A character vector of the query parameters to be passed  to
    #'    the API endpoint.
    #' @param name The parameter name
    #' @param val The user provided input
    #' @param class Accepted input classes
    #' @param length Accepted input lengths
    #' @inheritParams search_dates
    #' @return The \code{safe_api_search} function returns an object of class
    #'    \code{\link{safe_record_set}} of datasets that match the submitted
    #'    query. The \code{validate_query_param} function either returns NULL
    #'    on success or raises an error.
    #' @describeIn safe_api_search Constructs, submits and formats calls to
    #'    the SAFE API.
    #' @keywords internal

    # All of the search_* functions ultimately need the data index to construct
    # the safe_record_set, so ensure the safedata_dir is set:
    get_data_dir()

    # construct query string - note that the use in the search_* functions
    # of c(name = value, name = value) automatically drops NULL values.
    params <- paste(names(params), params, sep = "=", collapse = "&")

    if (!is.null(ids)) {
        # convert ids to safe_record_set if needed
        if (!inherits(ids, "safe_record_set")) {
            ids <- validate_record_ids(ids)
            if (!nrow(ids)) {
                stop("No valid record identifiers found in ids")
            }
        } else if (nrow(ids) == 0) {
            stop("Empty record set passed as ids")
        }

        # reduce to record ids (not concept ids)
        ids <- ids$record[!is.na(ids$record)]
        ids <- paste("ids", ids, sep = "=", collapse = "&")
        params <- paste0(params, "&", ids)
    }

    if (most_recent) {
        params <- paste0(params, "&most_recent=")
    }

    url <- getOption("safedata.url")
    url <- sprintf("%s/%s/%s?%s", url, "api/search", endpoint, params)
    url <- utils::URLencode(url)
    # Get the URL content as a response object
    content <- try_to_download(url)

    if (isFALSE(content)) {
        message("Search API unavailable:")
        message(attr(content, "fail_msg"))
        return(invisible(NULL))
    } else {
        # extract the content - auto conversion from JSON to list
        content <- httr::content(content)
    }

    verbose_message(sprintf("Search returned %i records", content$count))

    # convert search results to safe_record_set
    if (content$count > 0) {
        recids <- sapply(content$entries, "[[", "zenodo_record_id")
        ret <- validate_record_ids(recids)
    } else {
        ret <- data.frame(
            concept = numeric(0), record = numeric(0),
            available = logical(0), most_recent = numeric(0),
            mra = numeric(0)
        )
        class(ret) <- c("safe_record_set", "data.frame")
    }

    return(ret)
}


validate_query_param <- function(name, val, class = "character", length = 1) {
    #' @describeIn safe_api_search A basic query parameter validation handler
    #' @keywords internal

    if (!is.null(val) &&
        (!inherits(val, class) || !length(val) %in% length)) {
        msg <- paste0(
            "Parameter %s must be of length %s and ",
            "have one of the following classes: %s"
        )
        stop(sprintf(
            msg, name, paste(length, collapse = ","),
            paste(class, collapse = ",")
        ))
    }
}
