# Turn on the example safedata directory, which also turns on the mocking
# of URLs for API calls
library(safedata)
set_example_safedata_dir(on = TRUE)

# Turn it off again after tests run.
withr::defer(set_example_safedata_dir(on = FALSE), teardown_env())


local_create_sdd <- function(dir = tempfile(), env = parent.frame()) {
    # create new safedata directory
    create_safedata_dir(dir, url = "http://example.safedata.server")
    withr::defer(fs::dir_delete(dir), envir = env)

    return(dir)
}
