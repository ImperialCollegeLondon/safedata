#!/bin/bash
# Script to only rebuild the package documentation

# 1) Move down into the source directory and update 
#    the documents using Roxygen and pkgdown
cd ../
Rscript -e "devtools::document()"
Rscript -e " pkgdown::clean_site()"
Rscript -e " pkgdown::build_site()"
