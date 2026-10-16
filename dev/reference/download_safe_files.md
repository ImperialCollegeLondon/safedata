# Download SAFE dataset files

This downloads files associated with safedata datasets, either all of
the files included in a set of records (`xlsx_only = FALSE`) or just the
core .xlsx files (`xlsx_only = FALSE`), and stores them in the safedata
directory. See
[`insert_dataset`](https://imperialcollegelondon.github.io/safedata/dev/reference/insert_dataset.md)
for details on using embargoed or restricted datasets.

## Usage

``` r
download_safe_files(
  record_ids,
  confirm = TRUE,
  xlsx_only = TRUE,
  download_metadata = TRUE,
  refresh = FALSE,
  token = NULL
)
```

## Arguments

- record_ids:

  A vector of safedata dataset record ids or a
  [`safe_record_set`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
  object.

- confirm:

  Requires the user to confirm before download (logical)

- xlsx_only:

  Should all files be downloaded or just the core .xslx file (logical)

- download_metadata:

  Should the metadata record for the file be downloaded (logical)

- refresh:

  Should the function check if local copies have been modified and
  download fresh copies. This is useful if the local copies have
  unintentionally been modified but note the warning above.

- token:

  An access token for restricted datasets. These tokens are requested
  through the Zenodo page for a restricted dataset and are long
  alphanumeric strings. If you are providing a token, you should only
  provide the record id for that dataset.

## Value

Invisibly, a vector of paths within the \`safe_dir\` for successfully
downloaded files. If the download is not successful then the function
returns FALSE.

## Details

By default, the function will also download the dataset metadata. This
information is required by many of the functions in the package but
users can turn off automatic metadata download.

## Warning

Using `refresh = TRUE` will **overwrite locally modified files** and
replace them with the versions of record from Zenodo. You should not, in
general, modify files within a safedata directory.

## Examples

``` r
   # \donttest{
       set_example_safedata_dir()
       # Validate records to download
       recs <- validate_record_ids(c(3247631, 3266827, 3266821))
       print(recs)
#> Set includes 3 concept ids and 2 record ids: 
#>  - 2 open and most recent (*)
#>  - 0 open and outdated (o)
#>  - 0 under embargo or restricted (x)
#> 
#>   concept  record available
#> 1 3247630 3247631         *
#> 2 3266821 -------          
#> 3 3266826 3266827         *
       download_safe_files(recs, confirm = FALSE)
#> 2 files requested from 2 records
#>  - 0 local (0 bytes)
#>  - 0 embargoed or restricted (0 bytes)
#>  - 2 to download (675 Kb)
#> Downloading 2 record metadata files
#> Downloaded metadata: 3247631
#> Downloaded metadata: 3266827
#> 1 files for record 3247631: 1 to download
#>  - Downloaded: Both_tree_functional_traits.xlsx
#> 1 files for record 3266827: 1 to download
#>  - Downloaded: template_Symes.xlsx
       set_example_safedata_dir(on = FALSE)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
   # }
   if (FALSE) { # \dontrun{
       # This example requires a private token
       download_safe_files(1237730, confirm = FALSE,
                           token="longStringFromZenodo")
   } # }
```
