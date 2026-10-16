# Load and cache the safedata gazetteer

This function loads the gazetteer, stored as a geojson file in the root
of the safedata directory, as an
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) GIS object. The
GIS data uses the WGS84 (EPSG:4326) geographic coordinate system.

## Usage

``` r
load_gazetteer()
```

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
containing the gazetteer locations.

## Details

The gazetteer contains the following fields:

- location:

  The official gazetteer name for a sampling site.

- type:

  A short description of location type - typically the project that
  created the location

- plot_size:

  Where applicable, the size of the plot at a location. Note that point
  locations may define a sampling area with a plot size.

- display_order:

  DELETE

- parent:

  DELETE

- region:

  One of the four major large scale sampling areas: SAFE, Maliau, Danum
  and VJR

- fractal_order:

  Only defined for the SAFE core sampling points, which follow a fractal
  layout.

- transect_order:

  Again, for SAFE core sampling points, the location of a point along
  the sampling design transect.

- centroid_x, centroid_y:

  The centroid of the feature

- source:

  The original source GIS file that the feature was described in.

- bbox_xmin, bbox_ymin, bbox_xmax, bbox_ymax:

  The bounding box of the feature.

- geometry:

  The GIS geometry for the data - a column of class
  [`sfc`](https://r-spatial.github.io/sf/reference/sfc.html).

When this function is first called in a session, the loaded
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object is
cached for re-use (see
[`load_index`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)).

## See also

[`load_location_aliases`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_location_aliases.md),
[`load_index`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)
