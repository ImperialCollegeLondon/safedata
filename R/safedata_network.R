# This file contains functionality to support graceful failure of safedata
# actions if an internet connection is not available or the safedata.url is
# not available. Graceful failure (not an error or warning) is a requirement
# for CRAN packages.

# The implementation borrows heavily from discussion here:
# https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199/4
# and the implementation of that discussion in:
# https://github.com/jlacko/RCzechia/

# Summary of when connection is required and appropriate action:
#
# index.R
# - set_safe_dir(create = TRUE): cannot create new dir without network
# - set_safe_dir(update = TRUE): cannot update indices - 'offline' mode but done
#   atomically so that the index files are always left in new or old config.
#
#   These are accessing the index, gazetter, location aliases and file hashes
#   APIs, and we cannot assume that a single check within set_safe_dir is
#   sufficient as the connection could fail at any time, so needs to be graceful
#   at the file level.
#
# metadata.R
# - fetch_record_metata: cannot retrieve JSON data from api/record: 'offline'
#   mode
#
# load_safe.R
# - download actual data files - offline mode, not worrying too much about
#   partial metadata downloads, these don't cause problems
#
# taxa.R
# - download taxon coverage - offline mode.


#' Network resources and the safedata package.
#'
#' @description
#' The safedata package requires access to the internet in order to:
#'
#' 1. maintain an up-to-date index of datasets and the locations gazeteer,
#' 2. download datasets and their metadata,
#' 3. download metadata about the taxonomic coverage of datasets, and
#' 4. search dataset metadata for relevant datasets.
#'
#' These actions use two resources. The first is the safedata web server API
#' which provides everything except the actual datasets. The second is the
#' Zenodo API, which provides the datasets.
#'
#' If you do not have an internet connection - or if either of the two APIs is
#' unavailable - the safedata package can be used as normal to load and use
#' datasets that have already been downloaded to the local safedata directory.
#' However, it will not be possible to update the dataset index, download new
#' datasets or search for datasets until the APIs are available. The package
#' should handle internet failures gracefully and provide meaningful messages.
#'
#' @section Note on SSL certificates:
#'
#' Downloading data uses the curl package and the underlying libcurl library.
#' Some older versions of Mac OS X (10.14 and earlier) provide a built-in
#' libcurl with an outdated set of certificates that prevents curl from
#' connecting to resources using LetsEncrypt for HTTPS, which includes
#' https://safeproject.net. To use safedata on these systems, you have to
#' install a newer version of curl (e.g. using brew) and then compile curl
#' from source, linking it to that newer libcurl. The simplest way to do this
#' is to use \code{export PKG_CONFIG_PATH="/usr/local/opt/curl/lib/pkgconfig"}
#' before installing the package: this points the installation to the package
#' configuration for the brew installed version of curl.
#'
#' @name safedata_network
NULL

#' Attempt to download a URL resource, failing gracefully.
#'
#' This function tries to fetch a resource from a URL and handles failure to
#' resolve the resource (bad URLs), timeouts and then actual HTTP error
#' codes.
#'
#' If the download fails, the function returns FALSE and the return value
#' attribute 'fail_msg' is used to provide details. Otherwise, an
#' \code{\link[httr]{response}} object is returned containing the resource. If a
#' local path is provided, the resource is downloaded to that path and the
#' function returns TRUE to indicate success.
#'
#' @section Note:
#'
#' This function contains code to simulate network failures of varying kinds (no
#' network, no API, specific resource unavailable) for use in unit testing.
#'
#' @param url The URL to download.
#' @param local_path A path to a file in which to save the URL content.
#' @param timeout The waiting time in seconds before a request should
#'    timeout.
#' @return An \code{response} object or a boolean showing if the
#'    download attempt was successful.
#' @keywords internal

try_to_download <- function(url, local_path = NULL, timeout = 10) {
    # Dummy variables used to implement unit testing of network failures
    network_down <- as.logical(Sys.getenv("NETWORK_DOWN", unset = FALSE))
    url_down <- as.logical(Sys.getenv("URL_DOWN", unset = FALSE))
    resource_down <- Sys.getenv("RESOURCE_DOWN", unset = NA)

    # Create a failure object, which will have attr for failure messages.
    fail <- FALSE

    # Is there a network connection _at all_?
    if (!curl::has_internet() || network_down) {
        attr(fail, "fail_msg") <- "No internet connection."
        return(fail)
    }

    # Otherwise, is a resource _specifically_ being blocked for testing purposes
    if ((!is.na(resource_down) && grepl(resource_down, url)) || url_down) {
        attr(fail, "fail_msg") <- sprintf("URL error: 404")
        return(fail)
    }

    # If a local path is provided, convert to an httr::write_disk request
    # object, otherwise it stays as NULL and the request is not modified
    if (!is.null(local_path)) {
        local_path <- httr::write_disk(local_path)
    }

    # Try and download the URL
    response <- tryCatch(
        httr::GET(url = url, local_path, httr::timeout(timeout)),
        error = function(e) conditionMessage(e),
        warning = function(w) conditionMessage(w)
    )

    # Responses that are not httr response objects - error strings
    if (inherits(response, "character")) {
        if (grepl("Could not resolve host", response)) {
            # URL is garbage - cannot resolve
            attr(fail, "fail_msg") <- "URL not found"
            return(fail)
        } else if (grepl("Timeout was reached", response)) {
            # No timely response
            attr(fail, "fail_msg") <- "URL timed out"
            return(fail)
        } else if (grepl("SSL certificate problem", response)) {
            # Letsencrypt + old Mac OS?
            attr(fail, "fail_msg") <- "SSL issue: see ?safedata_network"
            return(fail)
        } else if (grepl("Failed to open file", response)) {
            # local_path file failure
            attr(fail, "fail_msg") <- response
            return(fail)
        } else {
            attr(fail, "fail_msg") <- paste0(
                "Unknown curl response: ", response
            )
            return(fail)
        }
    }

    # Check for failures
    if (httr::http_error(response)) {
        attr(fail, "fail_msg") <- sprintf("URL error: %s", response$status_code)
        return(fail)
    }

    # Now all is good so return the response or TRUE if the response was
    # saved to disk
    if (is.null(local_path)) {
        return(response)
    } else {
        return(TRUE)
    }
}
