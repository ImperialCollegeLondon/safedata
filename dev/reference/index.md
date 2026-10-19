# Package index

## Indexing and download functions

The `safedata` package maintains a local index of the datasets available
in a safedata dataset repository, used to check the availability of
datasets and to download and verify dataset files. These commands are
used to generate the local index, validate record ids against the index
and to download data.

- [`safedata-package`](https://imperialcollegelondon.github.io/safedata/dev/reference/safedata.md)
  [`safedata`](https://imperialcollegelondon.github.io/safedata/dev/reference/safedata.md)
  : Finding and using data from the SAFE Project.
- [`set_safedata_dir()`](https://imperialcollegelondon.github.io/safedata/dev/reference/set_safedata_dir.md)
  [`create_safedata_dir()`](https://imperialcollegelondon.github.io/safedata/dev/reference/set_safedata_dir.md)
  : Set or create a safedata directory
- [`validate_record_ids()`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
  [`print(`*`<safe_record_set>`*`)`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
  [`` `&`( ``*`<safe_record_set>`*`)`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
  [`` `|`( ``*`<safe_record_set>`*`)`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
  : Validates dataset record ids from user input
- [`download_safe_files()`](https://imperialcollegelondon.github.io/safedata/dev/reference/download_safe_files.md)
  : Download SAFE dataset files
- [`set_example_safedata_dir()`](https://imperialcollegelondon.github.io/safedata/dev/reference/set_example_safedata_dir.md)
  : Functions to use an example data directory for package examples

## Data discovery

These functions provide an interface to the dataset metadata search API
provided by the safedata_server web application.

- [`search_dates()`](https://imperialcollegelondon.github.io/safedata/dev/reference/search_safe.md)
  [`search_fields()`](https://imperialcollegelondon.github.io/safedata/dev/reference/search_safe.md)
  [`search_authors()`](https://imperialcollegelondon.github.io/safedata/dev/reference/search_safe.md)
  [`search_taxa()`](https://imperialcollegelondon.github.io/safedata/dev/reference/search_safe.md)
  [`search_text()`](https://imperialcollegelondon.github.io/safedata/dev/reference/search_safe.md)
  [`search_spatial()`](https://imperialcollegelondon.github.io/safedata/dev/reference/search_safe.md)
  : Metadata search functions for safedata datasets.

## Metadata display

A safedata dataset contains a considerable amount of metadata at the
dataset, data worksheet and field levels. These functions display the
metadata available at each of these levels.

- [`show_concepts()`](https://imperialcollegelondon.github.io/safedata/dev/reference/show_concepts.md)
  [`show_record()`](https://imperialcollegelondon.github.io/safedata/dev/reference/show_concepts.md)
  [`show_worksheet()`](https://imperialcollegelondon.github.io/safedata/dev/reference/show_concepts.md)
  : Show safedata metadata

## Loading data

These functions load data from a safedata dataset and allow users to
explore the taxa and locations associated with a dataset and to match
taxa and locations into a loaded data worksheet.

- [`load_safe_data()`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_safe_data.md)
  [`str(`*`<safedata>`*`)`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_safe_data.md)
  [`print(`*`<safedata>`*`)`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_safe_data.md)
  : Load data from a safedata dataset.
- [`insert_dataset()`](https://imperialcollegelondon.github.io/safedata/dev/reference/insert_dataset.md)
  : Inserts local copies of files from a dataset into a safedata
  directory
- [`get_taxa()`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxa.md)
  : Obtaining taxonomy for a safedata dataset.
- [`get_taxon_coverage()`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxon_coverage.md)
  : Retrieve the current taxon index
- [`igraph_to_phylo()`](https://imperialcollegelondon.github.io/safedata/dev/reference/igraph_to_phylo.md)
  [`get_phylogeny()`](https://imperialcollegelondon.github.io/safedata/dev/reference/igraph_to_phylo.md)
  : Getting a phylogeny for a dataset
- [`get_taxon_graph()`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxon_graph.md)
  : Get a graph of the taxa in a dataset
- [`add_taxa()`](https://imperialcollegelondon.github.io/safedata/dev/reference/add_taxa.md)
  : Add a taxonomic hierarchy to a SAFE data worksheet.
- [`get_locations()`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_locations.md)
  : Obtaining locations for a safedata dataset.
- [`add_locations()`](https://imperialcollegelondon.github.io/safedata/dev/reference/add_locations.md)
  : Adds location data to a SAFE data worksheet.

## Accessing files

These functions help users get access to the files stored in a safedata
directory.

- [`get_file_details()`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_file_details.md)
  : Get file details for a record

## Internal functions

The following internal functions are not included in the package
namespace but are documented here briefly.

- [`get_data_dir()`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_data_dir.md)
  : Checks the data directory is set and returns it
- [`verbose_message()`](https://imperialcollegelondon.github.io/safedata/dev/reference/verbose_message.md)
  : Message function that can be globally muted
- [`download_index()`](https://imperialcollegelondon.github.io/safedata/dev/reference/download_index.md)
  : Downloads the current dataset index
- [`download_gazetteer()`](https://imperialcollegelondon.github.io/safedata/dev/reference/download_gazetteer.md)
  : Downloads the gazetteer from a safedata metadata server.
- [`download_location_aliases()`](https://imperialcollegelondon.github.io/safedata/dev/reference/download_location_aliases.md)
  : Downloads location aliases from a safedata metadata server.
- [`safedata_env`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)
  [`load_index()`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)
  [`get_index()`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_index.md)
  : Load and cache the dataset index.
- [`load_gazetteer()`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_gazetteer.md)
  : Load and cache the safedata gazetteer
- [`load_location_aliases()`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_location_aliases.md)
  : Load and cache location aliases
- [`fetch_record_metadata()`](https://imperialcollegelondon.github.io/safedata/dev/reference/fetch_record_metadata.md)
  [`load_record_metadata()`](https://imperialcollegelondon.github.io/safedata/dev/reference/fetch_record_metadata.md)
  : Get and load safedata metadata
- [`safe_api_search()`](https://imperialcollegelondon.github.io/safedata/dev/reference/safe_api_search.md)
  [`validate_query_param()`](https://imperialcollegelondon.github.io/safedata/dev/reference/safe_api_search.md)
  : Internal SAFE dataset API search functions
- [`taxon_index_to_taxon_table()`](https://imperialcollegelondon.github.io/safedata/dev/reference/taxon_index_to_taxon_table.md)
  : Create a table showing taxon hierarchies from taxon metadata
- [`try_to_download()`](https://imperialcollegelondon.github.io/safedata/dev/reference/try_to_download.md)
  : Attempt to download a URL resource, failing gracefully.
- [`safedata_network`](https://imperialcollegelondon.github.io/safedata/dev/reference/safedata_network.md)
  : Network resources and the safedata package.
- [`mock_handler()`](https://imperialcollegelondon.github.io/safedata/dev/reference/mock_api.md)
  [`mock_api()`](https://imperialcollegelondon.github.io/safedata/dev/reference/mock_api.md)
  : Simple mocking of package URLS
- [`mock_api_urls`](https://imperialcollegelondon.github.io/safedata/dev/reference/mock_api_urls.md)
  : URLs handled by mock_api
