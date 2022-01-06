context("Test that taxon functions fail gracefully")
library(safedata)

test_that("no internet fails gracefully get_taxon_coverage", {
    Sys.setenv(NETWORK_DOWN = TRUE)

    set_example_safe_dir()

    success <- expect_message(
        get_taxon_coverage(),
        regexp = "Unable to download taxon index",
    )
    expect_null(success)

    unset_example_safe_dir()
    Sys.unsetenv("NETWORK_DOWN")
})

test_that("API down fails gracefully get_taxon_coverage", {
    Sys.setenv(URL_DOWN = TRUE)

    set_example_safe_dir()

    success <- expect_message(
        get_taxon_coverage(),
        regexp = "Unable to download taxon index",
    )
    expect_null(success)

    unset_example_safe_dir()
    Sys.unsetenv("URL_DOWN")
})
