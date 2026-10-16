# Validates dataset record ids from user input

This takes a vector of user supplied record identifiers and validates
them against the index. Typically the identifiers are provided as
integers, but the function will also handle Zenodo URLs and DOIs.

## Usage

``` r
validate_record_ids(record_set)

# S3 method for class 'safe_record_set'
print(x, ...)

# S3 method for class 'safe_record_set'
x & y

# S3 method for class 'safe_record_set'
x | y
```

## Arguments

- record_set:

  A vector of values containing Zenodo concept or record ids.

- x, y:

  Objects of class `safe_record_set`

- ...:

  Further arguments to print methods, unused.

## Value

An object of class `safe_record_set` (see Details)

## Details

The function returns a data frame with class `safe_record_set`,
containing the columns `concept`, `record`, `available` and, finally,
`mra` containing the most recent available record (if any). The function
can be run on an existing `safe_record_set` to update this information.

Note that `record` will be NA when a value represents a concept id.
Inputs that do not match a record or concept ids are returned in the
attribute `mismatches` of the record set.

This function is largely used internally to validate user inputs and to
provide a common output for the search functions but is exported to
allow users to check record ids and display summary information using
the print method.

## Functions

- `print(safe_record_set)`: Print a brief summary of "safe_record_set"
  objects.

- ` & `: Combine two record sets, retaining only records that are
  present in both.

- ` | `: Combine two record sets, including the records that are present
  in either.

## Examples

``` r
   set_example_safedata_dir()
   validate_record_ids(c(3247631, 3266827, 3266821, -1000))
#> Warning: Invalid numeric record ids: -1000
#> Set includes 3 concept ids and 2 record ids: 
#>  - 2 open and most recent (*)
#>  - 0 open and outdated (o)
#>  - 0 under embargo or restricted (x)
#> 
#>   concept  record available
#> 1 3247630 3247631         *
#> 2 3266821 -------          
#> 3 3266826 3266827         *
   validate_record_ids(c("https://doi.org/10.5281/zenodo.3247631",
                         "10.5281/zenodo.3266827",
                         "https://zenodo.org/record/3266821",
                         "not_this_one/3266821"))
#> Warning: Invalid character record ids: not_this_one/3266821
#> Set includes 3 concept ids and 2 record ids: 
#>  - 2 open and most recent (*)
#>  - 0 open and outdated (o)
#>  - 0 under embargo or restricted (x)
#> 
#>   concept  record available
#> 1 3247630 3247631         *
#> 2 3266821 -------          
#> 3 3266826 3266827         *
   set_example_safedata_dir(on=FALSE)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
```
