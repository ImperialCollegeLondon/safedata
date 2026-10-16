# Inserts local copies of files from a dataset into a safedata directory

If files are embargoed or restricted, then users may request the
datafiles from the authors. This function allows provided files to be
incorporated into a safedata directory, so that they will then work
seamlessly alongside openly available data.

## Usage

``` r
insert_dataset(record_id, files)
```

## Arguments

- record_id:

  A safedata dataset record id

- files:

  A vector of files to insert into the data directory

## Examples

``` r
   set_example_safedata_dir()
   files <- system.file("api_data", "template_ClareWfunctiondata.xlsx",
                        package = "safedata")
   insert_dataset(1237719, files)
#> Downloading 1 record metadata files
#> Downloaded metadata: 1237719
#> Inserting files: template_ClareWfunctiondata.xlsx
   dat <- load_safe_data(1237719, "Data")
   str(dat)
#> SAFE dataset
#> Concept: 1198840; Record 1237719; Worksheet: Data
#> 'data.frame':    34 obs. of  11 variables:
#>  $ taxon_name              : chr  "Anguilla.borneensis" "Anguilla.marmorata" "Barbodes.sealei" "Barbonymus.balleroides" ...
#>  $ Body_size               : num  24.14 30.56 8.51 11.36 7.59 ...
#>  $ body_shape              : Factor w/ 3 levels "Compressed","Cylindrical",..: 2 2 1 1 1 2 1 2 1 2 ...
#>  $ trophic_position        : Factor w/ 4 levels "Herbivore","Higher carnivore",..: 2 2 4 3 3 2 2 1 3 1 ...
#>  $ mouth_position          : Factor w/ 3 levels "Inferior","Superior",..: 3 3 3 3 3 3 3 1 3 1 ...
#>  $ presence_teeth          : Factor w/ 2 levels "no","yes": 2 2 1 1 2 2 1 1 1 1 ...
#>  $ gregariousness          : Factor w/ 2 levels "no","yes": 1 1 2 2 1 1 1 2 2 2 ...
#>  $ presence_barbels        : Factor w/ 2 levels "no","yes": 2 2 2 2 1 2 2 2 1 2 ...
#>  $ vertical_position_water : Factor w/ 4 levels "Benthic","Benthopelagic",..: 2 2 3 3 2 2 2 2 3 1 ...
#>  $ air.breathing_capability: chr  "yes" "yes" "no" "no" ...
#>  $ air-breathing_capability: Factor w/ 0 levels: NA NA NA NA NA NA NA NA NA NA ...
   set_example_safedata_dir(on=FALSE)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
```
