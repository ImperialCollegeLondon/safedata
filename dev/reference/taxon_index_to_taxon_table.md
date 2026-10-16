# Create a table showing taxon hierarchies from taxon metadata

Taxon data is stored as a set of taxa with taxon ids and parent taxon
ids. For any given dataset, the metadata includes an index containing
this information for all taxa used in data worksheets and all parent
taxa required. This internal function takes the taxon index metadata and
builds a table including the taxonomic hierarchy for each worksheet
taxon. It is used by
[`get_taxa`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxa.md)
and
[`get_taxon_coverage`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxon_coverage.md).

## Usage

``` r
taxon_index_to_taxon_table(taxon_index)
```

## Arguments

- taxon_index:

  A taxon index list from the dataset metadata,

## Value

A data frame adding higher taxon level information for each taxon

## Details

Note that this function does not discriminate between NCBI and GBIF
taxon indices: they are both mapped onto the same table structure.
Public calling functions should handle any merging and tidying required
for a neat public API.

## See also

[`get_taxa`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxa.md)
