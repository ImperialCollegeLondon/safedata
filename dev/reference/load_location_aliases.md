# Load and cache location aliases

This function loads location alias data, stored in CSV format in the
`location_aliases.csv` file in the root of the safedata directory (see
[`set_safedata_dir`](https://imperialcollegelondon.github.io/safedata/dev/reference/set_safedata_dir.md)).
The data is returned as a simple data frame.

## Usage

``` r
load_location_aliases()
```

## Value

A data frame containing location aliases.

## Details

When this function is first called in a session, the loaded data frame
object is cached for reuse (see
[`load_index`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)).

## See also

[`load_gazetteer`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_gazetteer.md),
[`load_index`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)
