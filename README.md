# safe_data
An R package for discovering and using data from the SAFE Project. For more information, please visit the [SAFE Project homepage](https://www.safeproject.net/), learn about how [SAFE Project data are handled](https://safe-dataset-checker.readthedocs.io/en/latest/), and browse the [SAFE Project database](https://zenodo.org/communities/safe/?page=1&size=20).

Further details for the `safe_data` package can be accessed at the [homepage](https://imperialcollegelondon.github.io/safe_data/index.html).

## Installing safe_data
There are two options for installing the package
1. Use `install_github` in the `devtools` package, i.e. `devtools::instsall_github("ImperialCollegeLondon/safe_data")`
2. Install the binary package from CRAN, i.e. `install.packages("safe_data")`

## Getting started
The two main functionalities of `safe_data` are for accessing and downloading SAFE data files, and importing them into R.

### 1. Download SAFE project data
Download data for record ID [3081059](https://zenodo.org/record/3081059#.XPfX9xZKhhE):
```r
library(safe_data)
SAFE_dir <- 'C:/Users/User/Documents/SAFE/'
record_ID <- 3081059
getSafe(record_ID, SAFE_dir)
```

### 2. Import SAFE project data into R
Import data file associated with record ID [3081059](https://zenodo.org/record/3081059#.XPfX9xZKhhE), named LiDAR_Aboveground_Carbon.xlsx:
```r
library(safe_data)
SAFE_dir <- 'C:/Users/User/Documents/SAFE/'
file_name <- 'LiDAR_Aboveground_Carbon.xlsx'
path_to_file <- file.path(SAFE_dir, concept_ID, record_ID, file_name)
record <- importSafe(path_to_file)
printSummary(path_to_file)
```

## Further examples
A more detailed description of the package and additional, more detailed examples can be found in the [package vignettes](https://imperialcollegelondon.github.io/safe_data/articles/).

## Building the package from source

The package uses [roxygen2](https://cran.r-project.org/web/packages/roxygen2/index.html) to maintain package documentation, so the build process is:

```sh
# Make sure the documentation is up to date
cd safe_data
Rscript -e 'devtools::document()' 
# Build the package
cd ../
R CMD BUILD safe_data
# Check the resulting package 
R CMD check safedata_0.1.tar.gz
```
