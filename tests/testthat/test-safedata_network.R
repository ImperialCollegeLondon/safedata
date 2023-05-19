context("Test that the safedata network comms fail gracefully")

test_that("no internet fails gracefully", {
    local_sdd <- local_create_sdd()

    Sys.setenv(NETWORK_DOWN = TRUE)
    success <- safedata:::try_to_download("https://httpbin.org")

    expect_false(success)
    expect_match(attr(success, "fail_msg"), regexp = "No internet connection")

    Sys.unsetenv("NETWORK_DOWN")
})


# test_that("bad host fails gracefully", {
#   TODO - requires raise mechanism in mocking
#     local_sdd <- local_create_sdd()
#     skip_if_no_internet()

#     success <- safedata:::try_to_download("https://url.invalid")

#     expect_false(success)
#     expect_match(attr(success, "fail_msg"), regexp = "URL not found")
# })

# test_that("timeout fails gracefully", {
#  TODO - requires timeout mechanism in mocking
#     local_sdd <- local_create_sdd()
#     skip_if_no_internet()

#     success <- safedata:::try_to_download(
#         "https://httpbin.org/delay/2",
#         timeout = 1
#     )

#     # If httpbin genuinely times out, the end result is the same.
#     expect_false(success)
#     expect_match(attr(success, "fail_msg"), regexp = "URL timed out")
# })

test_that("URL errors fails gracefully", {
    local_sdd <- local_create_sdd()
    skip_if_no_internet()

    success <- safedata:::try_to_download("https://httpbin.org/status/404")
    expect_false(success)

    # If a real httpbin call issues a Gateway timeout 504, then "URL error"
    # is still reported, but if it _properly_ times out then we get
    # "URL timed out" and that will cause a test failure unless trapped.
    if (!grepl("URL timed out", attr(success, "fail_msg"))) {
        expect_match(attr(success, "fail_msg"), regexp = "URL error")
    }
})

test_that("Good URL works and returns object to memory", {
    local_sdd <- local_create_sdd()
    skip_if_no_internet()

    success <- safedata:::try_to_download("https://httpbin.org/json")

    # Screen for failure of the download due to timeout
    if (is.logical(success) && isFALSE(success)) {
        skip("Download failed")
    }

    # Check we get content not logical
    expect_false(is.logical(success) && isTRUE(success))
    # Check the content is as expected
})

test_that("Bad path fails gracefully", {
    local_sdd <- local_create_sdd()
    skip_if_no_internet()

    success <- safedata:::try_to_download(
        "https://httpbin.org/json",
        local_path = "/does/not/exist"
    )

    expect_false(success)
    expect_match(attr(success, "fail_msg"), regexp = "Failed to open file")
})
