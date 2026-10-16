# Load data from a safedata dataset.

This function returns a data frame containing the data from a data
worksheet in a safedata dataset. Note that safedata `.xlsx` files
include other (non-data) worksheets Summary, Taxa, Locations that
contain metadata: see
[`get_taxa`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxa.md),
[`get_locations`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_locations.md),
[`add_taxa`](https://imperialcollegelondon.github.io/safedata/dev/reference/add_taxa.md)
and
[`add_locations`](https://imperialcollegelondon.github.io/safedata/dev/reference/add_locations.md)
for accessing and using this metadata.

## Usage

``` r
load_safe_data(record_id, worksheet, ...)

# S3 method for class 'safedata'
str(object, ...)

# S3 method for class 'safedata'
print(x, n = 10, ...)
```

## Arguments

- record_id:

  A safedata dataset record id

- worksheet:

  The name of the worksheet to load

- ...:

  For `load_safe_data`, these are additional arguments passed on to
  [`download_safe_files`](https://imperialcollegelondon.github.io/safedata/dev/reference/download_safe_files.md)
  if the data file needs to be downloaded. Otherwise, these are further
  arguments to the `str` and `print` methods.

- x, object:

  A `safedata` object.

- n:

  The number of rows to show in the `print` method.

## Value

A data frame with the additional `safedata` class and additional
attribute data containing metadata for the data.

## Details

In particular, the large amount of data worksheet summary metadata is
not attached as attributes to the data frame returned by this function.
This is largely to avoid excessive output to the console during normal
use of the data frame: an extended description of a worksheet can be
displayed using
[`show_worksheet`](https://imperialcollegelondon.github.io/safedata/dev/reference/show_concepts.md).

Currently, this function only loads data from safedata formatted `.xlsx`
files - data stored in external files is not yet handled.

## Functions

- `str(safedata)`: Display structure of a safedata data frame

- `print(safedata)`: Print safedata data frame

## Examples

``` r
   set_example_safedata_dir()
   beetle_abund <- load_safe_data(1400562, "Ant-Psel")
   str(beetle_abund)
#> SAFE dataset
#> Concept: 1400561; Record 1400562; Worksheet: Ant-Psel
#> 'data.frame':    20 obs. of  3 variables:
#>  $ Site                : chr  "SAFE_E" "SAFE_D" "SAFE_F" "SAFE_C" ...
#>  $ ant.species.richness: num  31 31 27 29 31 21 31 25 32 26 ...
#>  $ ant.abundance       : num  561 713 392 481 1036 ...
   # See also the show_worksheet function for further worksheet metadata
   show_worksheet(beetle_abund)
#> Record ID: 1400562
#> Worksheet name: Ant-Psel
#> Number of data rows: 20
#> Number of data fields: 3
#> Description:
#> Ant-Pselaphine data
#> Status: open
#> 
#> Fields:
#>   field_name           field_type description                    
#> 1 Site                 Location   Site where sample was collected
#> 2 ant species richness Numeric    Number of ant species in sample
#> 3 ant abundance        Abundance  Total number of ants in sample 
   set_example_safedata_dir(on=FALSE)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
```
