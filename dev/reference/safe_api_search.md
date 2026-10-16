# Internal SAFE dataset API search functions

The two internal functions described here handle validating the
parameters passed to the exported search functions and then constructing
API calls from the parameters and the common `ids` and `most_recent`
arguments.

## Usage

``` r
safe_api_search(endpoint, params, ids = NULL, most_recent = FALSE)

validate_query_param(name, val, class = "character", length = 1)
```

## Arguments

- endpoint:

  The name of the search API endpoint to be used.

- params:

  A character vector of the query parameters to be passed to the API
  endpoint.

- ids:

  A set of safedata dataset record IDs to restrict a search. This will
  typically be a
  [`safe_record_set`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
  object returned by another search but can also be a vector of record
  ids in any of the formats accepted by
  [`validate_record_ids`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md).

- most_recent:

  Logical indicating whether to restrict the API to returning only the
  most recent versions of the datasets found. By default all versions of
  matching dataset concepts are returned.

- name:

  The parameter name

- val:

  The user provided input

- class:

  Accepted input classes

- length:

  Accepted input lengths

## Value

The `safe_api_search` function returns an object of class
[`safe_record_set`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
of datasets that match the submitted query. The `validate_query_param`
function either returns NULL on success or raises an error.

## Functions

- `safe_api_search()`: Constructs, submits and formats calls to the
  \`safedata_server\` API.

- `validate_query_param()`: A basic query parameter validation handler
