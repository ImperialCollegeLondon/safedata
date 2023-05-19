context("Test that fetch metadata fail gracefully")

test_that("no internet fails gracefully on fetch", {
    local_sdd <- local_create_sdd()

    Sys.setenv(NETWORK_DOWN = TRUE)
    recs <- validate_record_ids(c(1237733, 1237731))

    success <- expect_message(
        safedata:::fetch_record_metadata(recs),
        regexp = "Failed to download metadata",
    )

    expect_false(success)
    Sys.unsetenv("NETWORK_DOWN")
})

test_that("no API fails gracefully on fetch", {
    local_sdd <- local_create_sdd()

    Sys.setenv(URL_DOWN = TRUE)
    recs <- validate_record_ids(c(1237733, 1237731))

    success <- expect_message(
        safedata:::fetch_record_metadata(recs),
        regexp = "Failed to download metadata",
    )

    expect_false(success)
    Sys.unsetenv("URL_DOWN")
})


test_that("API fails gracefully on partial fetch", {
    local_sdd <- local_create_sdd()

    Sys.setenv(RESOURCE_DOWN = "/api/record/1237731$")
    recs <- validate_record_ids(c(1237733, 1237731))

    success <- expect_message(
        safedata:::fetch_record_metadata(recs),
        regexp = "Failed to download metadata",
    )

    expect_false(success)
    Sys.unsetenv("RESOURCE_DOWN")
})

test_that("API works normally", {
    # This test checks the normal behaviour of the code, but will
    # cause a failure if there are network issues. So, it should run
    # during package development and testing - where network failures
    # can be checked and resolved - but should not run on CRAN so that
    # the package testing completes gracefully.
    local_sdd <- local_create_sdd()

    recs <- validate_record_ids(c(1237733, 1237731))

    success <- expect_message(
        safedata:::fetch_record_metadata(recs),
        regexp = "Downloaded metadata",
    )

    expect_true(success)
})
