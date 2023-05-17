context("Test that download SAFE data fails gracefully")

test_that("Download dataset fails gracefully on internet down", {
    # Use the local safe directory fixture
    local_sdd <- local_create_sdd()

    Sys.setenv(NETWORK_DOWN = TRUE)

    # Attempt to download a small dataset (otter qPCR), using confirm = FALSE
    # to avoid having to interact with the function.
    success <- expect_message(
        download_safe_files(1198475, confirm = FALSE),
        regexp = "Could not download metadata, aborting download",
    )
    expect_false(success)

    Sys.unsetenv("NETWORK_DOWN")
})

test_that("Download dataset fails gracefully on metadata server down", {
    # Use the local safe directory fixture
    local_sdd <- local_create_sdd()

    Sys.setenv(URL_DOWN = TRUE)

    # Attempt to download a small dataset (otter qPCR), using confirm = FALSE
    # to avoid having to interact with the function.
    success <- expect_message(
        download_safe_files(1198475, confirm = FALSE),
        regexp = "Could not download metadata, aborting download",
    )
    expect_false(success)

    Sys.unsetenv("URL_DOWN")
})

test_that("Download dataset fails gracefully on Zenodo down", {
    # This _specifically_ checks behaviour if Zenodo is down. If there is a
    # _real_ (not simulated) outage of the internet or SAFE database then this
    # will fail at the JSON metadata stage (test above), so _only_ run this if
    # the specific failure case can be simulated.

    if (!curl::has_internet()) {
        skip("No internet - skipping test")
    }

    # Use the local safe directory fixture
    local_sdd <- local_create_sdd()

    # Pre-fetch the metadata JSON, so that if the metadata server is down we can
    # skip the rest of the test.
    recs <- validate_record_ids(1198475)
    success <- safedata:::fetch_record_metadata(recs)
    if (!success) {
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
    Sys.unsetenv("RESOURCE_DOWN")
})
