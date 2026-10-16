# Simple mocking of package URLS

These functions allows a curated list of URLs used in the package to be
mocked for use in examples, vignettes and testing. This avoids the
package relying on web connections for testing and allows vignettes to
use actual live code when building, rather than having to fake those
code blocks. It also separates the safedata server API from package
development, so that package testing does not require an actual server
running an updated API, but can simulate it it locally.

## Usage

``` r
mock_handler(req)

mock_api(on = TRUE)
```

## Details

The `api_mock` function turns the HTTR mocking on and off, by adding the
`mock_handler` callback handler via
[`set_callback`](https://httr.r-lib.org/reference/get_callback.html).
This handler is very basic and it would be more robust to use something
like the webmockr package, but at present that does not support binary
mocked files and also (less importantly) loads the content of responses
into its registry rather than on request.

## Functions

- `mock_handler()`: A mock handler for URL callbacks
