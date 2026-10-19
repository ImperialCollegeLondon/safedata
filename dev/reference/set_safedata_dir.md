# Set or create a safedata directory

These functions set or create a local local safedata directory. This is
a strongly structured directory used to store safedata datasets, along
with record and index metadata. It is not intended to be modified by
users.

## Usage

``` r
set_safedata_dir(safedir, update = TRUE)

create_safedata_dir(
  safedir,
  url = "https://data.safeproject.net",
  use_zenodo_sandbox = FALSE
)
```

## Arguments

- safedir:

  A path to the directory to use as the safedata directory (str).

- update:

  Should the local dataset index be updated (logical)?

- url:

  A URL for a safedata metadata server.

- use_zenodo_sandbox:

  A boolean indicating whether the datafiles are stored in the main
  Zenodo site or the sandbox Zenodo site.

## Value

Invisibly, a boolean showing whether a safedata directory was created or
set successfully.

## Functions

- `create_safedata_dir()`: Create a new safedata directory

## The safedata directory structure

The safedata package uses a data directory to store local copies of
dataset files along with index files. Files for a dataset record are
stored in subdirectories using the zenodo concept id for the record and
then record id: `3342494/3342495`, for example. In addition to data
files from these records, these folders can also contain a JSON file
containing record metadata (e.g. `3342495.json`): this is a structured
version of the summary information shown on the Zenodo page.

The root of the data directory also holds three core resource files and
a settings file for the safedata directory:

- `index.json`:

  , containing a full list of the files and dataset records available in
  the safedata community for a project;

- `gazetteer.geojson`:

  , containing the official list of known sampling locations and GIS
  data; and

- `location_aliases.json`:

  , a list of alternative names permitted for some locations.

- `url.json`:

  , the URL settings for the safedata metadata server

## Creating a new safadata directory

The `create_safedata_dir` function will try to create the named
directory and requires an internet connection. The `url` argument must
provide URL to a website that provides the safedata metadata server API.
This URL will then be used to download the core index files described
above.

The default behaviour is to download actual data files from the main
Zenodo site, but a sandbox site is also provided for testing. The
sandbox site may be useful in setting up a new safedata archive, so
`use_zenodo_sandbox = TRUE` can be used to create a safedata directory
that will download data from the sandbox site.

## Metadata updates and validation

By default, the function will update the core index files to include
data on newly published datasets in the fata index. This requires an
internet connection to check for updates and a warning will be issued if
no connection is available - the `update = FALSE` option can be use to
suppress this message for offline use.

Setting a safedata directory will also trigger validation of the
directory structure. This will issue warnings when files other than
those found in datasets are present within the data structure or when
any dataset files that are present have been modified.
