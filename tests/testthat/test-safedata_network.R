context("Test that the safedata network comms fail gracefully")
library(safedata)

test_that("no internet fails gracefully", {

    Sys.setenv(NETWORK_DOWN = TRUE)
    success <- expect_message(
        safedata:::try_to_download("https://httpbin.org"),
        regexp = "No internet connection.",
    )
    expect_false(success)

    Sys.setenv(NETWORK_DOWN = FALSE)

})

test_that("bad host fails gracefully", {

    success <- expect_message(
        safedata:::try_to_download("https://httpbinzzzzz.org"),
        regexp = "URL not found",
    )
    expect_false(success)

})

test_that("timeout fails gracefully", {

    success <- expect_message(
        safedata:::try_to_download("https://httpbin.org/delay/2", timeout = 1),
        regexp = "URL timed out",
    )
    expect_false(success)

})

test_that("URL errors fails gracefully", {

    success <- expect_message(
        safedata:::try_to_download("https://httpbin.org/status/404"),
        regexp = "URL error",
    )
    expect_false(success)

})

test_that("Good URL works and returns object to memory", {

    success <-  safedata:::try_to_download("https://httpbin.org/base64/c2FmZWRhdGE=")

    # Screen for failure of the download (get FALSE, not a response object)
    expect_false(is.logical(success) && success == TRUE)
    # Check the content is as expected
    expect_equal(rawToChar(success$content), "safedata")

})

test_that("no internet and safedata dir creation fails gracefully", {

    Sys.setenv(NETWORK_DOWN = TRUE)
    temp_safe_dir <- file.path(tempdir(), 'test_safe_data')

    success <- expect_message(
        safedata::set_safe_dir(temp_safe_dir, create = TRUE),
        regexp = "Could not download required files: SAFE data directory not created",
    )

    expect_false(success)

    # Check it tidied up
    expect_false(file.exists(temp_safe_dir))

    Sys.setenv(NETWORK_DOWN = FALSE)

})

test_that("API down and safedata dir creation fails gracefully", {

    Sys.setenv(URL_DOWN = TRUE)
    temp_safe_dir <- file.path(tempdir(), 'test_safe_data')

    success <- expect_message(
        safedata::set_safe_dir(temp_safe_dir, create = TRUE),
        regexp = "Could not download required files: SAFE data directory not created",
    )

    expect_false(success)

    # Check it tidied up
    expect_false(file.exists(temp_safe_dir))

    Sys.setenv(URL_DOWN = FALSE)

})