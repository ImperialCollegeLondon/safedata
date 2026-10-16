# Retrieve the current taxon index

This function downloads the current taxon index across published
datasets and returns a formatted taxon table showing the taxonomic
hierarchy of each taxon, along with the taxonomic authority used for
validation (GBIF or NCBI). The returned data frame includes a count of
the number of datasets that include each taxon. The function requires an
internet connection.

## Usage

``` r
get_taxon_coverage()
```

## Value

A data frame containing taxonomic hierarchy data for each taxon in the
database taxon index. If the API is unavailable, the function returns
NULL.

## Details

Note that the use of "user" defined taxa can lead to some unusual
entries. There is nothing to stop a user defining "Gallus gallus" as a
"user" taxon with Animalia as a parent taxon.

## See also

[`get_taxa`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxa.md)

## Examples

``` r
    # \donttest{
    all_taxa <- get_taxon_coverage()
#> Unable to download taxon index: 
#> URL not found
    # }
```
