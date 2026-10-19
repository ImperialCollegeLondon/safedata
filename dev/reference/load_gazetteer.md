# Load and cache the safedata gazetteer

This function loads the gazetteer, stored as GeoJSON data in the
`gazetteer.geojson` file in the root of the safedata directory (see
[`set_safedata_dir`](https://imperialcollegelondon.github.io/safedata/dev/reference/set_safedata_dir.md)).

## Usage

``` r
load_gazetteer()
```

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
containing the gazetteer locations.

## Details

The data is loaded as an
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) GIS object,
using the WGS84 (EPSG:4326) geographic coordinate system. The gazetteer
will contain at least the following two fields but may also contain
project specific extra data.

- location:

  The official gazetteer name for a sampling site.

- geometry:

  The GIS geometry for the data - a column of class
  [`sfc`](https://r-spatial.github.io/sf/reference/sfc.html).

When this function is first called in a session, the loaded
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object is
cached for reuse (see
[`load_index`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)).

## See also

[`load_location_aliases`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_location_aliases.md),
[`load_index`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)
