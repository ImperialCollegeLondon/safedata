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
    Sys.unsetenv("NETWORK_DOWN")
})

test_that("no API fails gracefully on create", {
    Sys.setenv(URL_DOWN = TRUE)
    testsd <- file.path(tempdir(), "testsd")

    success <- expect_message(
        set_safe_dir(testsd, create = TRUE),
        regexp = "Could not download required files",
    )
    expect_false(success)
    Sys.unsetenv("URL_DOWN")
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
        regexp = "Unable to check for updates, using existing index files",
    )
    expect_true(success)
    unset_example_safe_dir()
    Sys.unsetenv("NETWORK_DOWN")
})

test_that("no API fails gracefully on update", {
    Sys.setenv(URL_DOWN = TRUE)
    test_sd <- set_example_safe_dir()

    success <- expect_message(
        set_safe_dir(test_sd, update = TRUE),
        regexp = "Unable to check for updates, using existing index files",
    )
    expect_true(success)
    unset_example_safe_dir()
    Sys.unsetenv("URL_DOWN")
})

test_that("Specific resource unavailable fails gracefully on update - atomic update.", {

    Sys.setenv(RESOURCE_DOWN = '/api/index$')
    test_sd <- set_example_safe_dir()

    # Nerf the index file by adding trailing spaces so that the
    # hashes don't match and the file therefore requires updating
    write(" ", file = file.path(test_sd, "index.json"), append = TRUE)

    success <- expect_message(
        set_safe_dir(test_sd, update = TRUE),
        regexp = "Unable to download updates, using existing index files",
    )
    expect_true(success)
    unset_example_safe_dir()
    Sys.unsetenv("RESOURCE_DOWN")
})

test_that("Update works otherwise", {
    test_sd <- set_example_safe_dir()

    success <- expect_message(
        set_safe_dir(test_sd, update = TRUE),
        regexp = "Index files successfully updated",
    )
    expect_true(success)
    unset_example_safe_dir()
})