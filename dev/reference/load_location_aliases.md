# Load and cache location aliases

This function loads location aliases, stored as a csv file in the root
of the safedata directory, as data frame.

## Usage

``` r
load_location_aliases()
```

## Value

A data frame containing location aliases.

## Details

When this function is first called in a session, the loaded data frame
object is cached for re-use (see
[`load_index`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)).

## See also

[`load_gazetteer`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_gazetteer.md),
[`load_index`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)
