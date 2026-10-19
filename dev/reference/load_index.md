# Load and cache the dataset index.

These functions are used to load and cache the dataset record index from
the `index.json` file in the safedata directory (see
[`set_safedata_dir`](https://imperialcollegelondon.github.io/safedata/dev/reference/set_safedata_dir.md)).

## Usage

``` r
safedata_env

load_index()

get_index()
```

## Value

The `load_index` function loads and caches the index data, checks the
availability of datasets and checks for user modifications to the
safedata directory structure but does not return the index data. Use the
`get_index` function to retrieve the data from the cache.

## Details

When any of the three core resource files (`index.json`,
`gazetteer.geojson` and `location_aliases.csv`) are loaded, the file
contents are cached in a separate R environment (`safedata_env`) to
reduce file loading while the package is being used. This environment is
not intended to be accessed by users and changing the contents will
break package functionality.

## Functions

- `safedata_env`: The caching environment for core resource data

- `load_index()`: Load and cache dataset index data.

- `get_index()`: Return the cached index data

## See also

[`load_location_aliases`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_location_aliases.md),
[`load_gazetteer`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_gazetteer.md)
