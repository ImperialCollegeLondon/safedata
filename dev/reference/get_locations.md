# Obtaining locations for a safedata dataset.

This function returns a spatial 'simple features'
([`sf`](https://r-spatial.github.io/sf/reference/sf.html)) object for a
dataset record, including basic information about locations for the
gazetteer if requested.

## Usage

``` r
get_locations(obj, gazetteer_info = FALSE)
```

## Arguments

- obj:

  A single record id, or an existing safedata dataframe.

- gazetteer_info:

  Should all the gazetteer fields be included in the returned
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object.

## Value

An object of classes "safe_locations", "sf" and "data.frame".

## Details

A safedata dataset recording observations from the field will typically
include a Locations worksheet, containing all of the location names used
in Locations type fields in data worksheets. These entries are matched
against existing location names in the curated gazetteer. Users can also
include "new" location names in their dataset, which do not necessarily
have coordinates. Processing locations works as follows:

1.  Known locations (`new_location = FALSE`) are matched against both
    the official location names in the gazetteer and the accepted
    aliases.

2.  New locations are first checked against the location aliases list to
    see if any new locations in this dataset have been matched to a
    gazetteer location. The gazetteer location is then used in
    preference to any new location data within the dataset.

3.  Finally, remaining new locations are assigned any feature geometry
    within the dataset. New locations do not necessarily have geometry
    data, so may have null or empty geometries.

## Note

Not all safedata datasets contain locations. In this case
`get_locations` will return NULL.

## See also

[`add_locations`](https://imperialcollegelondon.github.io/safedata/dev/reference/add_locations.md),
[`load_gazetteer`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_gazetteer.md),
[`load_location_aliases`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_location_aliases.md)

## Examples

``` r
   set_example_safedata_dir()
   locations <- get_locations(1400562)
   set_example_safedata_dir(on=FALSE)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
```
