library(jsonlite)
library(RCurl)

# get the Zenodo record data

record_id <- 1216040

get_zenodo <- function(record_id){
	
	zenodo_record <- fromJSON(getURL(paste0('https://zenodo.org/api/records/', record_id)))
	
	# Is it a valid record number - can get a 404 for unknown numbers or 
	# a 301 to redirect a concept id to the most recent record
	if(! is.null(zenodo_record$status)){
		if(zenodo_record$status == 404){
			stop(paste0('Could not retrieve record - ', zenodo_record$message))	
		} else if (zenodo_record$status == 301) {
			warning('Record id is a record concept id, returning most recent version')
			most_recent = strsplit(zenodo_record$location, '/')[[1]][4]
			return(get_zenodo(most_recent))
		}
	}
	
	# Is it a SAFE community dataset
	if(! 'safe' %in% zenodo_record$metadata$communities$id){
		stop(paste0('Record is not in the SAFE Zenodo community'))	
	}
	
	# Versioning - use to establish the most recent version and to look for 
	# open access earlier versions if the most recent is closed. Need to find out
	# from Zenodo how to enumerate versions from a single PID.
}
