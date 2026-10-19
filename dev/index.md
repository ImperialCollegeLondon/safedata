# The `safedata` research data management

The `safedata` package for R is the data discovery and download system
for research data published within the broader `safedata` system for
research data management. The `safedata` system is designed to manage
heterogeneous field datasets collected as part of a wider research
system, such as a widely used field site. The system has three
components:

- The [`safedata_validator`
  package](https://safedata-validator.readthedocs.io/en/stable/) is used
  to enforce metadata and standarding formats on research datasets,
  ensuring that data are reusable and well-documented. The
  `safedata_validator` package also handles the publication of datasets
  to the Zenodo data archive, ensuring data accessibility and longevity.

- The [`safedata_server` web
  application](https://github.com/ImperialCollegeLondon/safedata_server)
  provides data discovery by providing a searchable metadata database of
  published datasets, and also provides core resources across datasets,
  such as a gazetteer of sampling locations.

- The [`safedata`
  package](https://imperialcollegelondon.github.io/safedata/) itself
  then provides users with tools to search the metadata to find datasets
  of interest, download tools to maintain local copies of datasets of
  interest and tools to map spatial and taxonomic context onto datasets.

The `safedata` system was originally designed to manage the data outputs
from the [Stability of Altered Forest Ecosystems (SAFE)
Project](https://imperialcollegelondon.github.io/safedata/articles/safe_project.html).

## The `safedata` R package

For more information on using the `safedata` package in R, see:

- the
  [overview](https://imperialcollegelondon.github.io/safedata/articles/overview.html)
  vignette for the `safedata` system,
- the [user
  guide](https://imperialcollegelondon.github.io/safedata/articles/using_safe_data.html)
  for the package, and
- the `safedata` [function
  reference](https://imperialcollegelondon.github.io/safedata/reference/index.html),

## Building `safedata`

See the [package development
page](https://imperialcollegelondon.github.io/safedata/build_notes.html)
for more information on package structure and build process.
