context("Test that download SAFE data fails gracefully")
library(safedata)

test_that("Download dataset fails gracefully on internet down", {

    Sys.setenv(NETWORK_DOWN = TRUE)

    # Use the example safe directory, which explicitly doesn't update,
    # hence avoiding network issues on setting it!
    test_sd <- set_example_safe_dir()

    # Attempt to download a small dataset (otter qPCR), using confirm = FALSE
    # to avoid having to interact with the function.
    success <- expect_message(
        download_safe_files(1198475, confirm = FALSE),
        regexp = "Could not download metadata, aborting download",
    )
    expect_false(success)

    # remove the example directory
    unlink(test_sd, recursive = TRUE)
    Sys.unsetenv("NETWORK_DOWN")
})

test_that("Download dataset fails gracefully on metadata server down", {

    Sys.setenv(URL_DOWN = TRUE)

    # Use the example safe directory, which explicitly doesn't update,
    # hence avoiding network issues on setting it!
    test_sd <- set_example_safe_dir()

    # Attempt to download a small dataset (otter qPCR), using confirm = FALSE
    # to avoid having to interact with the function.
    success <- expect_message(
        download_safe_files(1198475, confirm = FALSE),
        regexp = "Could not download metadata, aborting download",
    )
    expect_false(success)

    # remove the example directory
    unlink(test_sd, recursive = TRUE)
    Sys.unsetenv("URL_DOWN")
})

test_that("Download dataset fails gracefully on Zenodo down", {

    # This _specifically_ checks behaviour if Zenodo is down. If there is a
    # _real_ (not simulated) outage of the internet or SAFE database then this
    # will fail at the JSON metadata stage (test above), so _only_ run this if
    # the specific failure case can be simulated.

    if (! curl::has_internet()) {
        skip("No internet - skipping test")
    }

    # Use the example safe directory, which explicitly doesn't update,
    # hence avoiding network issues on setting it!
    test_sd <- set_example_safe_dir()

    # Pre-fetch the metadata JSON, so that if the metadata server is down we can
    # skip the rest of the test.
    recs <- validate_record_ids(1198475)
    success <- fetch_record_metadata(recs)
    if (! success) {
        skip("Metadata server down - skipping test")
    }

    # OK - now have a local copy of the metadata, so won't be downloaded again
    # and can simulate the Zenodo failure.
    Sys.setenv(RESOURCE_DOWN = "www.zenodo.org")

    # Attempt to download a small dataset (otter qPCR), using confirm = FALSE
    # to avoid having to interact with the function.
    success <- expect_message(
        download_safe_files(1198475, confirm = FALSE),
        regexp = "Failed to download data:",
    )

    # No successfully downloaded file names
    expect_length(success, 0)

    # remove the example directory
    unlink(test_sd, recursive = TRUE)
    Sys.unsetenv("RESOURCE_DOWN")
})