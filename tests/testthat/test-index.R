context("Test that set_safe_dir fail gracefully")
library(safedata)

test_that("no internet fails gracefully on create", {
    Sys.setenv(NETWORK_DOWN = TRUE)
    testsd <- file.path(tempdir(), "testsd")

    success <- expect_message(
        set_safe_dir(testsd, create = TRUE),
        regexp = "Could not download required files",
    )
    expect_false(success)
    Sys.setenv(NETWORK_DOWN = FALSE)
})

test_that("no API fails gracefully on create", {
    Sys.setenv(URL_DOWN = TRUE)
    testsd <- file.path(tempdir(), "testsd")

    success <- expect_message(
        set_safe_dir(testsd, create = TRUE),
        regexp = "Could not download required files",
    )
    expect_false(success)
    Sys.setenv(URL_DOWN = FALSE)
})

test_that("BAD API url fails gracefully on create", {
    testsd <- file.path(tempdir(), "testsd")
    success <- expect_message(
        set_safe_dir(testsd, create = TRUE, 
                     url = "https://www.safeproject.netzzz"),
        regexp = "Could not download required files",
    )
    expect_false(success)
})


test_that("no internet fails gracefully on update", {
    Sys.setenv(NETWORK_DOWN = TRUE)
    test_sd <- set_example_safe_dir()

    success <- expect_message(
        set_safe_dir(test_sd, update = TRUE),
        regexp = "Cannot access index API - unable to update",
    )
    expect_false(success)
    unset_example_safe_dir()
    Sys.setenv(NETWORK_DOWN = FALSE)
})
