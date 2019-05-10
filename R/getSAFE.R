getSAFE <- function (recordId, dir='.') {
  #'
  
  # get the record info
  zenodoRecord <- getZenodoRecordInfo(recordId)
  
  # are we using a concept record ID?
  if (recordId == zenodoRecord$conceptrecid) {
    isConceptRecId <- TRUE
  } else {
    isConceptRecId <- FALSE
  }
  
  # do we have a cache directory for this concept ID? if not then build one
  if (!file.exists(paste0(dir, '/', zenodoRecord$conceptrecid))) {
    dir.create(paste0(dir, '/', zenodoRecord$conceptrecid))
  }
  
  # update and load the version cache
  message('safeR message: Updating version cache for concept ID ', 
          zenodoRecord$conceptrecid, '... ', appendLF=FALSE)
  updateVersionCache(zenodoRecord$conceptrecid, dir=dir)
  message('completed!')
  vers <- readRDS(file.path(dir, zenodoRecord$conceptrecid, 'versions.rds'))
  
  # check that versions are ordered by date (newest first)
  vers <- vers[order(as.Date(vers$date), decreasing=TRUE), ]
  
  # perform some checks on the requested record
  latestUnembargoed <- vers$recordId[which(vers$access=='open')[1]]
  if (isConceptRecId) {
    # just return the latest unembargoed version
    id <- latestUnembargoed
  } else {
    # run some additional checks on record status
    if (vers$access[vers$recordId==zenodoRecord$id] == 'embargoed') {
      # 1. if the record is embargoed, raise an error
      stop(paste0('SAFE access error: Record ID requested (', zenodoRecord$id, 
                  ') is embargoed, open date is ', 
                  vers$openDate[vers$recordId==zenodoRecord$id], 
                  '.\n  ID of most recent open record: ', latestUnembargoed))
    } else if (vers$access[vers$recordId==zenodoRecord$id] == 'closed') {
      # 2. if the record is closed, raise an error
      stop(paste0('SAFE access error: Record ID request (', zenodoRecord$id, 
                  ') is closed'))
    } else if (zenodoRecord$id != latestUnembargoed) {
      warning('A more recent version of the record you are trying to access is',
              ' available, ID: ', latestUnembargoed,
              call.=FALSE, immediate.=TRUE)
      id <- zenodoRecord$id
    }
  }
  
  return(zenodoRecord)
}

getZenodoRecordInfo <- function (recordId) {
  #' Get record info for desired version of SAFE project stored on Zenodo
  #' 
  #' This function returns a \code{list} object from an API call to the Zenodo
  #' database using the supplied SAFE project record ID number. The API call
  #' contains information required to access the appropriate SAFE data. The
  #' function will raise an error in the event that the record ID either does
  #' not exist on Zenodo, or is not located in the SAFE community, and flags a
  #' warning when a concept record ID is supplied (in which case the latest
  #' version of the project is returned).
  #' 
  #' @param recordId, SAFE project record ID number. This could be a concept
  #'   record ID or a DOI identifier for a specific version number
  #' @return \code{list} object containing information from the Zenodo API call

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
      warning('Record ID (', recordId, 
              ') is a concept record ID, returning most recent file submission',
              call.=FALSE, immediate.=TRUE)
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

updateVersionCache <- function (conceptId, dir) {
  #' Updates the SAFE version cache for the project with the given concept ID
  #' 
  #' @param conceptId, the concept record ID for the SAFE project
  #' @param dir, local SAFE cache directory
  #' @return a dataframe containing access status information for each version
  #'   of the specified SAFE project
  
  # use extended API to get version details from Zenodo
  versions <- fromJSON(
    getURL(paste0('https://zenodo.org/api/records/?q=conceptrecid:', conceptId,
                  '&all_versions&sort=-version&size=10&page=1')))
  
  # create a dataframe with record ID, date, and access status
  versionsDf <- data.frame('doi'=versions$hits$hits$doi,
                           'recordId'=as.character(
                             lapply(versions$hits$hits$doi, getRecordIdFromDoi)),
                           'date'=versions$hits$hits$created,
                           'access'=versions$hits$hits$metadata$access_right,
                           'openDate'=versions$hits$hits$metadata$embargo_date,
                           stringsAsFactors=FALSE)
  
  # save this locally in the concept ID cache
  saveRDS(versionsDf, file=file.path(dir, conceptId, 'versions.rds'))
}

downloadSafeFile <- function (recordId, dir='.') {
  #' Download SAFE project file using the given record ID
  
  
}

getRecordIdFromDoi <- function (DOI) {
  #' Extracts Record ID from Zenodo DOI link
  
  if (!grepl('zenodo', DOI)) {
    stop('Please supply a valid Zenodo-sourced DOI')
  }
  return(substr(strsplit(DOI, 'zenodo')[[1]][2], start=2, stop=9999))
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