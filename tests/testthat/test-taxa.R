context("Test that taxon functions fail gracefully")

test_that("no internet fails gracefully get_taxon_coverage", {
    local_sdd <- local_create_sdd()

    Sys.setenv(NETWORK_DOWN = TRUE)
    success <- expect_message(
        get_taxon_coverage(),
        regexp = "Unable to download taxon index",
    )
    expect_null(success)

    Sys.unsetenv("NETWORK_DOWN")
})

test_that("API down fails gracefully get_taxon_coverage", {
    local_sdd <- local_create_sdd()

    Sys.setenv(URL_DOWN = TRUE)

    success <- expect_message(
        get_taxon_coverage(),
        regexp = "Unable to download taxon index",
    )
    expect_null(success)

    Sys.unsetenv("URL_DOWN")
})

test_that("Otherwise get_taxon_coverage works", {
    local_sdd <- local_create_sdd()

    result <- get_taxon_coverage()
    expect_s3_class(result, "data.frame")
})
