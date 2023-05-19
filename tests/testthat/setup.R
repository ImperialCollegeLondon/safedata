library(safedata)


# This functionality should extended with a switch to allow use of
# an actual live metadata server and test Zenodo community
# https://github.com/ImperialCollegeLondon/safedata/issues/10


# Turn on the mocking of URLs for API calls
safedata:::mock_api(on = TRUE)
# Turn it off again after tests run.
withr::defer(safedata:::mock_api(on = FALSE), teardown_env())

# Fixtures

local_create_sdd <- function(dir = tempfile(), env = parent.frame()) {
    #' Create a safedata directory, using the mocked server URL
    #' and then delete it again when a test completes.
    create_safedata_dir(dir, url = "http://example.safedata.server")
    withr::defer(unlink(dir, recursive = TRUE), envir = env)

    return(dir)
}

# Helper functions

skip_if_no_internet <- function() {
    #' Some tests rely on having the internet available to generate
    #' the various error messages from safedata:::try_to_download()
    #' via httpbin, so should not be run if there is a network outage.
    #' However, they _can_ run if the URLs are being mocked, so this
    #' function tests for no internet _and_ no request callback handler.

    if ((!curl::has_internet()) && is.null(httr::get_callback("request"))) {
        skip("No internet - skipping test")
    }
}
