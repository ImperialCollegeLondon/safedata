#!/bin/bash
# Script to build and check safedata

# 1) Move down into the source directory and update the documents using Roxygen and pkgdown
cd ../
Rscript -e "devtools::document()"
Rscript -e " pkgdown::clean_site()"
Rscript -e " pkgdown::build_site()"


# 2) Move down again into the parent directory and 
#    build the package from the source directory
cd ../
R CMD BUILD safedata

# 3) Identify the version name that just got built and check it for CRAN
VERSION=$(sed -n -e '4p' safedata/DESCRIPTION  | cut -d " "  -f 2)
R CMD CHECK --as-cran --run-donttest safedata_$VERSION.tar.gz 
