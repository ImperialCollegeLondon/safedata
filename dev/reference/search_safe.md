# Metadata search functions for safedata datasets.

In addition to the datasets stored on Zenodo, the safedata system
provides the \`safedata_server\` web application that serves an API to
search dataset metadata in more depth. The search functions access this
API and return
[`safe_record_set`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
objects identifying datasets that match a particular query.

## Usage

``` r
search_dates(dates, match_type = "intersect", most_recent = FALSE, ids = NULL)

search_fields(
  field_text = NULL,
  field_type = NULL,
  ids = NULL,
  most_recent = FALSE
)

search_authors(author, ids = NULL, most_recent = FALSE)

search_taxa(
  taxon_name = NULL,
  taxon_rank = NULL,
  taxon_id = NULL,
  taxon_auth = NULL,
  ids = NULL,
  most_recent = FALSE
)

search_text(text, ids = NULL, most_recent = FALSE)

search_spatial(
  wkt = NULL,
  location = NULL,
  distance = NULL,
  ids = NULL,
  most_recent = FALSE
)
```

## Arguments

- dates:

  A vector of length 1 or 2, containing either ISO format date character
  strings ("yyyy-mm-dd") or `POSIXt` dates.

- match_type:

  A character string (see Details).

- most_recent:

  Logical indicating whether to restrict the API to returning only the
  most recent versions of the datasets found. By default all versions of
  matching dataset concepts are returned.

- ids:

  A set of safedata dataset record IDs to restrict a search. This will
  typically be a
  [`safe_record_set`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
  object returned by another search but can also be a vector of record
  ids in any of the formats accepted by
  [`validate_record_ids`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md).

- field_text:

  Text to search for within the data worksheet field name and
  description.

- field_type:

  A data worksheet field type (see Links).

- author:

  A character string used to search for datasets by author full (or
  partial) names.

- taxon_name:

  The scientific name of a taxon to search for.

- taxon_rank:

  A taxonomic rank to search for.

- taxon_id:

  A numeric taxon ID number from GBIF or NCBI.

- taxon_auth:

  One of GBIF or NCBI. If not specified, results will match against taxa
  validated against either taxonomy database.

- text:

  Character string to look for within safedata datasets, worksheets,
  titles, field descriptions, and dataset keywords.

- wkt:

  A well-known text geometry string, assumed to use latitude and
  longitude in WGS84 (EPSG:4326).

- location:

  The name of a location in the gazetteer.

- distance:

  A buffer distance for spatial searches, giving the distance in metres
  within which to match either location or wkt searches.

## Value

An object of class
[`safe_record_set`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
of datasets that match the query.

## Details

The API provides endpoints to search datasets by date extents, data
worksheet fields, authors, taxa, free text and by spatial query. All of
the functions accept the argument `most_recent`, which restricts the
returned datasets to the most recent versions of each matching dataset
concept. The functions can also be passed an existing
[`safe_record_set`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
object to search within the results of a previous search.

The `match_type` parameter specifies how to match date ranges and must
be one of "intersect" (default), "contain", or "within". The "contain"
option returns datasets that span a date range, "within" returns
datasets that fall within the given range and "intersect" selects
datasets that overlap any part of the date range. Note that match_type
is ignored when only a single date is provided.

## Functions

- `search_dates()`: Search datasets by date extent

- `search_fields()`: Search data worksheet field metadata.

- `search_authors()`: Search by dataset author

- `search_taxa()`: Search by taxon name, rank or taxon ID.

- `search_text()`: Search dataset, worksheet and field titles and
  descriptions

- `search_spatial()`: Search by spatial sampling area/named location.

## Spatial searches

For spatial searches, users can select a location name from the project
gazetteer (see e.g. <https://data.safeproject.net/gazetteer/map/> or
[`load_gazetteer`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_gazetteer.md))
or provide a WKT geometry. The sampling locations provided in each
dataset are tested to see if they intersect the search geometry.

A buffer `distance` can aso be provided to extend the search around the
query geometry. Note that although WKT geometries should be provided
using WGS84 lat/long coordinates, since this is typical field GPS data,
distances must be provided as metres and all proximity calculations take
place in the UTM50N projected coordinate system.

The `search_spatial` function will not retrieve datasets that have not
provided sampling locations or use newly defined locations that are
missing coordinate information.

## Links

- the safedata_server API:

  e.g. <https://data.safeproject.net/api/docs/>

- Worksheet field types:

  <https://safedata-validator.readthedocs.io/en/stable/data_providers/data_format/data.html#field-types>

- Project gazetteer:

  See
  [`load_gazetteer`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_gazetteer.md)
  and e.g. <https://data.safeproject.net/gazetteer/map/>

- WKT:

  <https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry>

## Examples

``` r
# \donttest{
search_dates("2014-06-12")
#> Search API unavailable:
#> URL not found
search_dates(as.POSIXct(c("2014-06-12", "2015-06-11")))
#> Search API unavailable:
#> URL not found
search_dates(c("2014-06-12", "2015-06-11"), match_type = "contain")
#> Search API unavailable:
#> URL not found
search_fields(field_text = "temperature")
#> Search API unavailable:
#> URL not found
search_fields(field_type = "numeric")
#> Search API unavailable:
#> URL not found
search_fields(field_text = "temperature", field_type = "numeric")
#> Search API unavailable:
#> URL not found
search_authors("Ewers")
#> Search API unavailable:
#> URL not found
search_taxa(taxon_name = "Formicidae")
#> Search API unavailable:
#> URL not found
search_taxa(taxon_id = 4342, taxon_auth = "GBIF")
#> Search API unavailable:
#> URL not found
search_taxa(taxon_rank = "family")
#> Search API unavailable:
#> URL not found
search_text("forest")
#> Search API unavailable:
#> URL not found
search_text("ant")
#> Search API unavailable:
#> URL not found
search_spatial(wkt = "Point(116.5 4.75)")
#> Search API unavailable:
#> URL not found
search_spatial(wkt = "Point(116.5 4.75)", distance = 100000)
#> Search API unavailable:
#> URL not found
search_spatial(wkt = "Polygon((110 0, 110 10,120 10,120 0,110 0))")
#> Search API unavailable:
#> URL not found
search_spatial(location = "A_1")
#> Search API unavailable:
#> URL not found
search_spatial(location = "A_1", distance = 2500)
#> Search API unavailable:
#> URL not found

# combining searches using logical operators
fish <- search_taxa("Actinopterygii")
#> Search API unavailable:
#> URL not found
odonates <- search_taxa("Odonata")
#> Search API unavailable:
#> URL not found
ewers <- search_authors("Ewers")
#> Search API unavailable:
#> URL not found
aquatic <- fish | odonates
aquatic_ewers <- aquatic & ewers
all_in_one <- (fish | odonates) & ewers
# }
```
