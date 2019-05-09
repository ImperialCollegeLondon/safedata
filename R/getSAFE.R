getSAFE <- function (recordId, dir='.') {
  #'
  
  # get the record info
  zenodoRecord <- getZenodoRecordInfo(recordId)
  
  # do we have a cache for this concept ID? if not then build one
  if (!file.exists(paste0(dir, '/', zenodoRecord$conceptrecid))) {
    dir.create(paste0(dir, '/', zenodoRecord$conceptrecid))
  }
  
  
  
  # # check for the most recent version of the project
  # if (checkVersion) {
  #   
  # }

  # print(zenodoRecord$conceptrecid)
  
  return(zenodoRecord)
}

getZenodoRecordInfo <- function (recordId) {
  #' Get record info for desired version of SAFE project stored on Zenodo

  recordApi <- 'https://zenodo.org/api/records/'
  zenodoRecord <- fromJSON(getURL(paste0(recordApi, recordId)))
  
  # if an invalid record ID is passed, this should return an error message (404)
  # if a valid concept ID has been passed, this should return a redirect (301)
  if (!is.null(zenodoRecord$status)) {
    # check for either 404 (error) or 301 (redirect) message
    if (zenodoRecord$status == 404) {
      stop(paste0('Could not retrieve record (ID: "', 
                  recordId, '") - ', zenodoRecord$message))
    } else if (zenodoRecord$status == 301) {
      warning('Record ID is a concept record ID, returning most recent version')
      zenodoRecord <- 
        getZenodoRecordInfo(strsplit(zenodoRecord$location, '/')[[1]][4])
    }
  }
  
  # is it a SAFE community dataset?
  if(!'safe' %in% zenodoRecord$metadata$communities$id){
    stop(paste0('Record is not in the SAFE Zenodo community'))	
  }
  
  # return the zenodoRecord list object
  return(zenodoRecord)
}

checkSafeCache <- function (conceptId) {
  
}

updateVersionCache <- function (conceptId, dir) {
  #' Updates the SAFE version cache for the project with the given concept ID
  #' 
  #' @param conceptId, the concept record ID for the SAFE project
  #' @param dir, local SAFE cache directory
  #' @returns a dataframe containing access status information for each version
  #'   of the specified SAFE project
  
  # use extended API to get version details from Zenodo
  versions <- fromJSON(
    getURL(paste0('https://zenodo.org/api/records/?q=conceptrecid:', conceptId,
                  '&all_versions&sort=-version&size=10&page=1')))
  
  # create a dataframe with record ID, date, and access status
  versionsDf <- data.frame('doi'=versions$hits$hits$doi,
                           'date'=versions$hits$hits$created,
                           'access'=versions$hits$hits$metadata$access_right,
                           'openDate'=versions$hits$hits$metadata$embargo_date)
  
  # save this locally
  save(versionsDf, file=file.path(dir, conceptId, 'versions.RData'))
}

downloadSafeFile <- function (recordId, dir='.') {
  #' Download SAFE project file using the given record ID
  
  
}

# getSAFE <-
# function(record_ids, dir='.'){
# 
# api <- "https://sandbox.zenodo.org/api/records/%s"
# 
# for(rec in record_ids){
# record <- fromJSON(url(sprintf(api, record_id)))
# bucket <- fromJSON(record$links$bucket)
# 
# fname <- file.path(dir, basename(bucket$contents$links$self))
# download.file(bucket$contents$links$self, destfile=fname) 
# }
# }