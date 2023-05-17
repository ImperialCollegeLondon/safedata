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
    # Main index files
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
    # Example datasets used and metadata JSON
    "http://example.safedata.server/api/record/1400562" = list(
        content = "1400562.json",
        status_code = 200L
    ),
    "https://www.zenodo.org/record/1400562/files/Psomas_Ant_Pselaphine_SAFE_dataset.xlsx" = list(
        content = "Psomas_Ant_Pselaphine_SAFE_dataset.xlsx",
        status_code = 200L
    ),
    "http://example.safedata.server/api/record/3247631" = list(
        content = "3247631.json",
        status_code = 200L
    ),
    "https://www.zenodo.org/record/3247631/files/Both_tree_functional_traits.xlsx" = list(
        content = "Both_tree_functional_traits.xlsx",
        status_code = 200L
    ),
    "http://example.safedata.server/api/record/3266827" = list(
        content = "3266827.json",
        status_code = 200L
    ),
    "https://www.zenodo.org/record/3266827/files/template_Symes.xlsx" = list(
        content = "template_Symes.xlsx",
        status_code = 200L
    ),
    "http://example.safedata.server/api/record/1237719" = list(
        content = "1237719.json",
        status_code = 200L
    ),
    # Search API examples
    "http://example.safedata.server/api/search/text?text=soil" = list(
        content = "text_text_soil.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/search/taxa?name=Formicidae" = list(
        content = "taxa_name_Formicidae.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/search/taxa?gbif_id=4342" = list(
        content = "taxa_gbif_id_4342.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/search/spatial?location=BL_A" = list(
        content = "spatial_location_BL_A.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/search/spatial?wkt=POINT(116.97394%204.73481)&distance=2000" = list(
        content = "spatial_wkt_point_distance_2000.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/search/taxa?name=Actinopterygii" = list(
        content = "taxa_name_Actinopterygii.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/search/taxa?name=Odonata" = list(
        content = "taxa_name_Odonata.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/search/authors?name=Ewers" = list(
        content = "authors_name_Ewers.json",
        status_code = 200L
    ),
    "http://example.safedata.server/api/search/authors?name=Ewers&ids=1237719&ids=4072959&ids=3982665&ids=1478526&ids=3926374&ids=3974971&ids=6368114&ids=5820902&ids=5710509&ids=6477764" = list(
        content = "authors_ewers_within_ids.json",
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
    #' \code{\link[httr:set_callback]{set_callback}}.
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
