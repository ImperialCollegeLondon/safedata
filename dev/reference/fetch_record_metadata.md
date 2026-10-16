# Get and load safedata metadata

Internal handlers to:

## Usage

``` r
fetch_record_metadata(record_set)

load_record_metadata(record_set)
```

## Arguments

- record_set:

  An object of class
  [`safe_record_set`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md).

## Value

The `fetch_record_metadata` function invisibly returns a logical value
indicating if all records were fetched successfully and
`load_record_metadata` returns a list object containing record metadata

## Details

1.  ensure there are local copies of record metadata, fetching it using
    the safedata_server API if needed and

2.  load thatdata from file.

This is the same metadata used to populate the Zenodo description but is
downloaded as JSON format and stored within the record directory in the
safedata directory for reuse.

## Functions

- `fetch_record_metadata()`: Download and store JSON metadata for a
  record

- `load_record_metadata()`: Load JSON metadata for a record

## Examples

``` r
   # \donttest{
     rec <- validate_record_ids(1400562)
     safedata:::fetch_record_metadata(rec)
     metadata <- safedata:::load_record_metadata(rec)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
   # }
```
