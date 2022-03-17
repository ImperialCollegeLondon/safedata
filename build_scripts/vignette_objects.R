library(safedata)

# CRAN does _not_ build vignettes, but _does_ check to see that the code in
# vignettes runs. This creates a problem for graceful failure on internet access
# problems, because the objects created rely on some downloads, and can break
# the flow of the code.
#
# This file is used to generate an internal package dataset using R/sysdata.rda
# that contains objects used in building vignettes and then the internet usage
# in building the vignette is mocked.

set_example_safe_dir()

vignette_soil_search <- search_text('soil')
vignette_ants_search <- search_taxa('Formicidae')
vignette_taxon_coverage <- get_taxon_coverage()

save(vignette_soil_search,
     vignette_ants_search,
     vignette_taxon_coverage,
     file='../R/sysdata.rda',
     version=2)

unset_example_safe_dir()