# Turn on the mocking of URLs for API calls
library(safedata)
safedata:::mock_api(on = TRUE)

# Turn it off again after tests run.
withr::defer(safedata:::mock_api(on = FALSE), teardown_env())


local_create_sdd <- function(dir = tempfile(), env = parent.frame()) {
    # create new safedata directory, using the mocked server URL
    create_safedata_dir(dir, url = "http://example.safedata.server")
    withr::defer(fs::dir_delete(dir), envir = env)

    return(dir)
}
