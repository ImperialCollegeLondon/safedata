context("Test that set_safe_dir fail gracefully")

test_that("no internet fails gracefully on create", {
    Sys.setenv(NETWORK_DOWN = TRUE)
    testsd <- file.path(tempdir(), "testsd")

    success <- expect_message(
        create_safedata_dir(testsd),
        regexp = "Could not download required files",
    )
    expect_false(success)
    Sys.unsetenv("NETWORK_DOWN")
})

test_that("no API fails gracefully on create", {
    Sys.setenv(URL_DOWN = TRUE)
    testsd <- file.path(tempdir(), "testsd")

    success <- expect_message(
        create_safedata_dir(testsd),
        regexp = "Could not download required files",
    )
    expect_false(success)
    Sys.unsetenv("URL_DOWN")
})

test_that("BAD API url fails gracefully on create", {
    testsd <- file.path(tempdir(), "testsd")
    success <- expect_message(
        create_safedata_dir(testsd,
            url = "https://www.safeproject.netzzz"
        ),
        regexp = "Could not download required files",
    )
    expect_false(success)
})

test_that("Create succeeds otherwise", {
    testsd <- file.path(tempdir(), "testsd")
    success <- expect_message(
        create_safedata_dir(testsd,
            url = "http://example.safedata.server"
        ),
        regexp = "Safe data directory created",
    )
    expect_true(success)

    # Tidy up
    unlink(testsd, recursive = TRUE)
})


test_that("no internet fails gracefully on update", {
    local_sdd <- local_create_sdd()

    Sys.setenv(NETWORK_DOWN = TRUE)

    success <- expect_message(
        set_safedata_dir(local_sdd, update = TRUE),
        regexp = "Unable to download updates, using existing index files",
    )
    # defaults to existing local copy, so expect true
    expect_true(success)
    Sys.unsetenv("NETWORK_DOWN")
})

test_that("no API fails gracefully on update", {
    local_sdd <- local_create_sdd()

    Sys.setenv(URL_DOWN = TRUE)

    success <- expect_message(
        set_safedata_dir(local_sdd, update = TRUE),
        regexp = "Unable to download updates, using existing index files",
    )
    # defaults to existing local copy, so expect true
    expect_true(success)
    Sys.unsetenv("URL_DOWN")
})

test_that("Specific resource unavailable fails gracefully on update - atomic update.", {
    local_sdd <- local_create_sdd()

    Sys.setenv(RESOURCE_DOWN = "/api/metadata_index.json$")

    # Nerf the index file by adding trailing spaces so that the
    # hashes don't match and the file therefore requires updating
    write(" ", file = file.path(local_sdd, "index.json"), append = TRUE)

    success <- expect_message(
        set_safedata_dir(local_sdd, update = TRUE),
        regexp = "Unable to download updates, using existing index files",
    )
    # defaults to existing local copy, so expect true
    expect_true(success)
    Sys.unsetenv("RESOURCE_DOWN")
})

test_that("Update works otherwise", {
    # This test checks the normal behaviour of the code, but will
    # cause a failure if there are network issues. So, it should run
    # during package development and testing - where network failures
    # can be checked and resolved - but should not run on CRAN so that
    # the package testing completes gracefully.

    local_sdd <- local_create_sdd()

    success <- expect_message(
        set_safedata_dir(local_sdd, update = TRUE),
        regexp = "Index files successfully updated",
    )
    expect_true(success)
})
