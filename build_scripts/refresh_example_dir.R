library(safedata)
setwd('../inst/safedata_example_dir/')
path <- "safedata_example_dir"
# create the new directory and add the example file
set_safe_dir(path, create=TRUE)
download_safe_files(1400562, confirm=FALSE)
# remove the old archive
file.remove('safedata_example_dir.zip')
# create the new one from the updated directory
zip('safedata_example_dir.zip', path)
# remove the directory, leaving just the refreshed archive
unlink(path, recursive=TRUE)