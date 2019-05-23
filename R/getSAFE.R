zenodoRecordApiLookup <- function (id) {
  #' Return information of the given id using the Zenodo record API
  #' 
  #' This function returns a \code{list} object from an API call to the Zenodo
  #' cloud database using the supplied ID number. The \code{list} object
  #' contains varied record information for the ID.
  #' 
  #' @param id, the data ID number, which in this instance should be either a
  #'   SAFE project DOI or concept ID number
  #' @return \code{list} object containing information from the Zenodo API call
  #' @note Zenodo Record API: \url{https://zenodo.org/api/records/}
  
  return(jsonlite::fromJSON(RCurl::getURL(
    paste0('https://zenodo.org/api/records/', id))))
}

zenodoVersionsApiLookup <- function (id) {
  #' Perform a lookup on the Zenodo database with an API to access information
  #' for all versions of the specified id.
  #' 
  #' This function returns a \code{list} onject from an API call to the Zenodo
  #' cloud database using the supplied record ID number. The returned 
  #' \code{list}contains information on all available versions of the data
  #' assigned to that ID. Note that this API call only returns a populated 
  #' \code{list} if a concept record ID is passed, otherwise the \code{list} is
  #' empty.
  #' 
  #' @param, id, the data ID number, which in this instance should be a concept
  #'   record ID
  #' @return \code{list} object containing information on all versions of the ID
  #'   from the Zenodo API call
  #' @note Zenodo Versions API example: 
  #'   \url{https://zenodo.org/api/records/?q=conceptrecid:1198692&all_versions&sort=-version&size=10&page=1}
  
  return(jsonlite::fromJSON(RCurl::getURL(
    paste0('https://zenodo.org/api/records/?q=conceptrecid:', id, 
           '&all_versions&sort=-version&size=10&page=1'))))
}

isSafeProject <- function (zenodoRecord) {
  #' checks whether the supplied zenodo record is from the SAFE community
  
  if (!is.null(zenodoRecord$metadata)) {
    # in this case a record ID has been passed
    if ('safe' %in% zenodoRecord$metadata$communities$id) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (!is.null(zenodoRecord$hits)) {
    # in this case a concept record ID has been passed
    if ('safe' %in% zenodoRecord$hits$hits$metadata$communities[[1]]$id) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    # we have an unknown type
    return(FALSE)
  } 
}

updateSafeVersionCache <- function (versions, dir) {
  #' Updates the SAFE version cache for the project with the given concept ID
  #' 
  #' @param versions, the full versions list from the Zenodo 'versions' API call
  #' @param dir, local SAFE cache directory
  #' @return a dataframe containing access status information for each version
  #'   of the specified SAFE project
  #' @seealso \code{\link{zenodoVersionsApiLookup}}
  
  # create a dataframe with record ID, date, and access status
  versionsDf <- data.frame('doi'=versions$hits$hits$doi,
                           'recordId'=as.character(
                             lapply(versions$hits$hits$doi, getRecordIdFromDoi)),
                           'createdOn'=as.Date(versions$hits$hits$created),
                           'accessRight'=versions$hits$hits$metadata$access_right,
                           'openDate'=versions$hits$hits$metadata$embargo_date,
                           stringsAsFactors=FALSE)
  
  # order the versions by date (newest first)
  versionsDf <- versionsDf[order(versionsDf$createdOn, decreasing=TRUE), ]
  
  # save this locally in the concept ID cache
  saveRDS(versionsDf, file=file.path(dir, versions$hits$hits$conceptrecid[1],
                                     'versions.rds'))
}

getRecordIdFromDoi <- function (DOI) {
  #' Extracts Record ID from Zenodo DOI link
  
  if (!grepl('zenodo', DOI)) {
    stop('Please supply a valid Zenodo-sourced DOI')
  }
  return(substr(strsplit(DOI, 'zenodo')[[1]][2], start=2, stop=9999))
}

processSafe <- function (id, dir='.') {
  #' Get record info for desired version of SAFE project stored on Zenodo
  #' 
  #' This function returns a \code{list} object from an API call to the Zenodo
  #' database using the supplied SAFE project record ID number. The API call
  #' contains information required to access the appropriate SAFE data. The
  #' function will raise an error in the event that the record ID either does
  #' not exist on Zenodo, or is not located in the SAFE community, and flags a
  #' warning when a concept record ID is supplied (in which case the latest
  #' version of the project is returned -- NOTE that the latest version of the
  #' project may be "embargoed", so further checks will then be needed to 
  #' access the most recent "open" project file).
  #' 
  #' @param id, SAFE project record ID number. This could be a concept record
  #'   ID or a DOI identifier for a specific version number
  #' @return \code{list} object containing information from the Zenodo API call
  
  # get record info using standard API call
  record <- zenodoRecordApiLookup(id)
  
  if (!is.null(record$status)) {
    # check for either 404 (error) or 301 (redirect) message
    if (record$status == 404) {
      stop(paste0('Could not retrieve record (ID: ', id, ') - ', record$message))
    } else if (record$status == 301) {
      warning('Supplied ID (', id, ') is a concept record ID, attempting to ', 
              'return most recent file submission', call.=FALSE, immediate.=TRUE)
      isConceptRecId <- TRUE
      conceptRecId <- id
      allVersions <- zenodoVersionsApiLookup(id)
    }
  } else {
    # a valid record ID has been passed
    isConceptRecId <- FALSE
    conceptRecId <- record$conceptrecid
    message('Requested record (ID: ', id, ') is associated with concept record',
            ' ID ', conceptRecId)
    allVersions <- zenodoVersionsApiLookup(record$conceptrecid)
  }
  
  # check that the project is part of the SAFE community
  if (!isSafeProject(allVersions)) {
    stop(paste0('Concept ID ', conceptRecId, ' is not in the SAFE community'))
  }
  
  # do we have a cache directory for this concept ID? if not then build one
  if (!file.exists(file.path(dir, conceptRecId))) {
    dir.create(file.path(dir, conceptRecId))
  }
  
  # update and load the version cache
  message('Updating version cache for concept record ID ', conceptRecId, '... ', 
          appendLF=FALSE)
  updateSafeVersionCache(allVersions, dir=dir)
  message('complete!')
  vers <- readRDS(file.path(dir, conceptRecId, 'versions.rds'))
  
  # perform access checks on the requested record
  # records may be embargoed (raise an error)
  # records may be closed (raise an error)
  # records may not be the latest available version (raise a warning)
  latestOpen <- vers[which(vers$accessRight == 'open')[1],]
  if (isConceptRecId) {
    if (is.na(latestOpen$recordId)) {
      stop(paste0('No open records found for concept record ID ', conceptRecId, 
                  '. Current record status:\n', paste0(
                    capture.output(print(vers[,2:5], row.names=FALSE, right=FALSE)),
                    collapse='\n')), call.=FALSE)
    } else {
      return(zenodoRecordApiLookup(latestOpen$recordId))
    }
  } else {
    printVers <- vers[,2:5]
    printVers[' '] <- rep('', nrow(vers))
    printVers[printVers$recordId==record$id, ' '] <- '  <-REQUESTED'
    if (record$metadata$access_right %in% c('embargoed', 'closed')) {
      if (!is.na(latestOpen$recordId)) {
        # there are other records that are open
        stop(paste0('Requested record (ID: ', record$id, ') is ', 
                    record$metadata$access_right, '. Please select an open ',
                    'record for this concept ID. Current record status:\n',
                    paste0(
                      capture.output(print(printVers, row.names=FALSE, right=FALSE)),
                      collapse='\n')), call.=FALSE)
      } else {
        # there are no other open records
        stop(paste0('Requested record (ID: ', record$id, ') is ', 
                    record$metadata$access_right, '. There are no other open ',
                    'records for this concept ID. Current record status:\n',
                    paste0(
                      capture.output(print(printVers, row.names=FALSE, right=FALSE)),
                      collapse='\n')), call.=FALSE)
      }
    } else if (latestOpen$recordId != record$id) {
      # a more recent version is available
      warning(paste0('A more recent version of the requested record is ',
                     'available. Current record status:\n',
                     paste0(
                       capture.output(print(printVers, row.names=FALSE, right=FALSE)),
                       collapse='\n')), call.=FALSE, immediate.=TRUE)
    }
    return(record)
  }
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