% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_safe.R
\name{search_safe}
\alias{search_safe}
\alias{search_dates}
\alias{search_fields}
\alias{search_authors}
\alias{search_taxa}
\alias{search_text}
\alias{search_spatial}
\title{SAFE dataset search functions.}
\usage{
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
}
\arguments{
\item{dates}{A vector of length 1 or 2, containing either ISO format date
character strings ("yyyy-mm-dd") or \code{POSIXt} dates.}

\item{match_type}{A character string (see Details).}

\item{most_recent}{Logical indicating whether to restrict the API to
returning only the most recent versions of the datasets found. By default
all versions of matching dataset concepts are returned.}

\item{ids}{A set of SAFE dataset record IDs to restrict a search. This will
typically be a \code{\link{safe_record_set}} object returned by another
search but can also be a vector of record ids in any of the formats
accepted by \code{\link{validate_record_ids}}.}

\item{field_text}{Text to search for within the data worksheet field name
and description.}

\item{field_type}{A data worksheet field type (see Links).}

\item{author}{A character string used to search for datasets by author
full (or partial) names.}

\item{taxon_name}{The scientific name of a taxon to search for.}

\item{taxon_rank}{A taxonomic rank to search for.}

\item{taxon_id}{A numeric taxon ID number from GBIF or NCBI.}

\item{taxon_auth}{One of GBIF or NCBI. If not specified, results will match
against taxa validated against either taxonomy database.}

\item{text}{Character string to look for within a SAFE dataset, worksheet,
title, field description, and dataset keywords.}

\item{wkt}{A well-known text geometry string, assumed to use latitude and
longitude in WGS84 (EPSG:4326).}

\item{location}{The name of a location in the SAFE gazetteer.}

\item{distance}{A buffer distance for spatial searches, giving the distance
in metres within which to match either location or wkt searches.}
}
\value{
An object of class \code{\link{safe_record_set}} of datasets that
   match the query.
}
\description{
In addition to the datasets stored on Zenodo, the SAFE Project website
provides an API to search dataset metadata in more depth. The search
functions access this API and return \code{\link{safe_record_set}} objects
identifying datasets that match a particular query.
}
\details{
The API provides endpoints to search datasets by date extents, data
worksheet fields, authors, taxa, free text and by spatial query. All
of the functions accept the argument \code{most_recent}, which restricts
the returned datasets to the most recent versions of each matching dataset
concept. The functions can also be passed an existing
\code{\link{safe_record_set}} object to search within the results
of a previous search.

The \code{match_type} parameter specifies how to match date ranges and must
be one of "intersect" (default), "contain", or "within". The "contain" option
returns datasets that span a date range,  "within" returns datasets that
fall within the given range and "intersect" selects datasets that overlap any
part of the date range. Note that match_type is ignored when only a single
date is provided.
}
\section{Functions}{
\itemize{
\item \code{search_dates()}: Search datasets by date extent

\item \code{search_fields()}: Search data worksheet field metadata.

\item \code{search_authors()}: Search by dataset author

\item \code{search_taxa()}: Search by taxon name, rank or taxon ID.

\item \code{search_text()}: Search dataset, worksheet and field titles
and descriptions

\item \code{search_spatial()}: Search by spatial sampling area/named location.

}}
\section{Spatial searches}{

For spatial searches, users can select a location name from a SAFE
data gazetteer (see e.g. \url{https://www.safeproject.net/info/gazetteer}
or \code{\link{load_gazetteer}}) or provide a WKT geometry. The sampling
locations provided in each SAFE dataset are tested to see if they intersect
the search geometry.

A buffer \code{distance} can aso be provided to extend the search around the
query geometry. Note that although WKT geometries should be provided
using WGS84 lat/long coordinates, since this is typical field GPS data,
distances must be provided as metres and all proximity calculations take
place in the UTM50N projected coordinate system.

The \code{search_spatial} function will not retrieve datasets that have not
provided sampling locations or use newly defined locations that are missing
coordinate information.
}

\section{Links}{

\describe{
   \item{SAFE data API}{e.g. \url{https://www.safeproject.net/api}}
   \item{Worksheet field
   types}{\url{https://safedata-validator.readthedocs.io/en/latest/data_format/data.html#field-types}}  # nolint
   \item{SAFE gazetteer}{See \code{\link{load_gazetteer}} and e.g.
         \url{https://www.safeproject.net/info/gazetteer}}
   \item{WKT}{\url{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}}  # nolint
}
}

\examples{
\donttest{
search_dates("2014-06-12")
search_dates(as.POSIXct(c("2014-06-12", "2015-06-11")))
search_dates(c("2014-06-12", "2015-06-11"), match_type = "contain")
search_fields(field_text = "temperature")
search_fields(field_type = "numeric")
search_fields(field_text = "temperature", field_type = "numeric")
search_authors("Ewers")
search_taxa(taxon_name = "Formicidae")
search_taxa(taxon_id = 4342, taxon_auth = "GBIF")
search_taxa(taxon_rank = "family")
search_text("forest")
search_text("ant")
search_spatial(wkt = "Point(116.5 4.75)")
search_spatial(wkt = "Point(116.5 4.75)", distance = 100000)
search_spatial(wkt = "Polygon((110 0, 110 10,120 10,120 0,110 0))")
search_spatial(location = "A_1")
search_spatial(location = "A_1", distance = 2500)

# combining searches using logical operators
fish <- search_taxa("Actinopterygii")
odonates <- search_taxa("Odonata")
ewers <- search_authors("Ewers")
aquatic <- fish | odonates
aquatic_ewers <- aquatic & ewers
all_in_one <- (fish | odonates) & ewers
}
}
