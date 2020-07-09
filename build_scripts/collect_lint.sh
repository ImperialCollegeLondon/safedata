#!/bin/bash
# This script runs linting across the code and saves the output in the 
# package root. Note that this uses the .lintr file also in package root
# to configure linting and manage exceptions.

cd ../
Rscript -e "lintr::lint_package()" > lint.txt