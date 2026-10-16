# Get file details for a record

This function returns a data frame of the files associated with a
record. The access status of the record is reported, along with the
local absolute file path and whether a local copy is present. If there
is no local copy and the dataset is open access, you can use
[`download_safe_files`](https://imperialcollegelondon.github.io/safedata/dev/reference/download_safe_files.md)
to get local copies. If the dataset is embargoed or restricted, see
[`insert_dataset`](https://imperialcollegelondon.github.io/safedata/dev/reference/insert_dataset.md).

## Usage

``` r
get_file_details(record_id)
```

## Arguments

- record_id:

  A record id

## Value

A data frame with fields: filename, dataset_access, path and local.

## Examples

``` r
   # \donttest{
   set_example_safedata_dir()
   files <- get_file_details(1400562)
   set_example_safedata_dir(on=FALSE)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
   # }
```
