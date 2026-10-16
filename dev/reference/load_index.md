# Load and cache the dataset index

This function loads the dataset record index from the JSON file in the
safedata directory and sets which datasets are currently available and
which records are the most recent available under a given concept. The
function also checks to see if there are any user modifications to the
directory structure.

## Usage

``` r
load_index()
```

## Value

Returns NULL invisibly - use get_index() to obtain the index data.

## Details

When any of the three index files (`index.json`, `gazetteer.geojson` and
`location_aliases.csv`) are loaded, the file contents are cached in
memory to reduce load times. The cache is within an environment that is
not exported in the package namespace and is not intended to be user
accessible.

## See also

[`load_location_aliases`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_location_aliases.md),
[`load_gazetteer`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_gazetteer.md)
