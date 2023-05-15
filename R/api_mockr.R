#' URLs handled by mock_api
#'
#' This list provides a set of URLs handled by the \code{mock_api} URL
#' mocking function and handler. The list provides the matched URL along with
#' the name of the file (in api \code{inst/api_data} directory) used to
#' provide any content in the response body and the status code to be
#' returned.
#'
#' @keywords internal

mock_api_urls <- list(
    "http://example.safedata.server/api/metadata_index.json" = list(
        content = "index.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/gazetteer.json" = list(
        content = "gazetteer.geojson",
        status_code = 200L
    ),
    "http://example.safedata.server/api/location_aliases.json" = list(
        content = "location_aliases.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/index_hashes.json" = list(
        content = "index_hashes.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/record/1400562" = list(
        content = "1400561/1400562/1400562.json",
        status_code = 200L
    )
)



mock_handler <- function(req) {
    #' @describeIn mock_api A mock handler for URL callbacks
    #' @keywords internal

    # Match the request URL against the configured mocked URLS
    mocked_response <- mock_api_urls[[req$url]]
    if (is.null(mocked_response)) {
        stop("Unknown URL passed to mock_handler: ", req$url)
    }

    # Handle the content to be used
    if (!is.null(mocked_response$content)) {
        # Get the path to the local canned response content
        response_file <- system.file(
            "api_data", mocked_response$content,
            package = "safedata"
        )

        # Handle the content:
        # - If the request uses httr::write_disk, then the request will have an
        #   output$path element, so copy to the path
        # - Otherwise replace the null content in the response body.
        if (!is.null(req$output$path)) {
            file.copy(response_file, req$output$path)
            content <- httr:::path(response_file)
        } else {
            content <- readBin(
                response_file,
                what = "raw", file.info(response_file)$size
            )
        }
    } else {
        # Null content for error statuses
        content <- raw(0)
    }

    # Build a very basic mocked response object
    response <- httr:::response(
        url = req$url,
        status_code = mocked_response$status_code,
        headers = list(
            "Content-Type" = "application/json",
            "content-type" = "application/json"
        ),
        all_headers = list(),
        cookies = list(),
        content = content,
        date = Sys.time(),
        times = numeric(0),
        request = req,
        handle = NULL
    )

    return(response)
}

mock_api <- function(on = TRUE) {
    #' Simple mocking of package URLS
    #'
    #' These functions allows a curated list of URLs used in the package
    #' to be mocked for use in examples, vignettes and testing. This avoids the
    #' package relying on web connections for testing and allows vignettes to
    #' use actual live code when building, rather than having to fake those
    #' code blocks. It also separates the safedata server API from package
    #' development, so that package testing does not require an actual server
    #' running an updated API, but can simulate it it locally.
    #'
    #' The \code{api_mock} function turns the HTTR mocking on and off, by
    #' adding the \code{mock_handler} callback handler via
    #' \code{\link{httr:set_callback}}.
    #' This handler is very basic and it would be more robust to use something
    #' like the \pkg{webmockr} package, but at present that does not support
    #' creating local files from responses, which is needed for mocking
    #' responses while building an example safedata directory.
    #'
    #' @keywords internal

    if (on) {
        httr::set_callback("request", mock_handler)
    } else {
        httr::set_callback("request")
    }
}
