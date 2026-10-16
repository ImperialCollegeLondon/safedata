# Attempt to download a URL resource, failing gracefully.

This function tries to fetch a resource from a URL and handles failure
to resolve the resource (bad URLs), timeouts and then actual HTTP error
codes.

## Usage

``` r
try_to_download(url, local_path = NULL, timeout = 10)
```

## Arguments

- url:

  The URL to download.

- local_path:

  A path to a file in which to save the URL content.

- timeout:

  The waiting time in seconds before a request should timeout.

## Value

An `response` object or a boolean showing if the download attempt was
successful.

## Details

If the download fails, the function returns FALSE and the return value
attribute 'fail_msg' is used to provide details. Otherwise, an
[`response`](https://httr.r-lib.org/reference/response.html) object is
returned containing the resource If a local path is provided, the
resource is downloaded to that path and the function returns TRUE to
indicate success.

## Note

This function contains code to simulate network failures of varying
kinds (no network, no API, specific resource unavailable) for use in
unit testing.
