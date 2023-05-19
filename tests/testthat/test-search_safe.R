context("Test that search SAFE fail gracefully")

test_that("no internet fails gracefully on search", {
    local_sdd <- local_create_sdd()

    Sys.setenv(NETWORK_DOWN = TRUE)

    success <- expect_message(
        search_text("ant"),
        regexp = "Search API unavailable",
    )
    expect_null(success)
    Sys.unsetenv("NETWORK_DOWN")
})

test_that("no API fails gracefully on search", {
    local_sdd <- local_create_sdd()

    Sys.setenv(URL_DOWN = TRUE)

    success <- expect_message(
        search_text("ant"),
        regexp = "Search API unavailable",
    )
    expect_null(success)
    Sys.unsetenv("URL_DOWN")
})

test_that("normal search behaviour", {
    # This test checks the normal behaviour of the code, but will
    # cause a failure if there are network issues. So, it should run
    # during package development and testing - where network failures
    # can be checked and resolved - but should not run on CRAN so that
    # the package testing completes gracefully.

    local_sdd <- local_create_sdd()

    success <- expect_message(
        search_text("soil"),
        regexp = "Search returned",
    )
    expect_true(inherits(success, "safe_record_set"))
})
