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
  #' 
  #' This function returns a boolean indicating whether the supplied Zenodo
  #' record information (the result of an API lookup to the Zenodo database) is 
  #' part of the SAFE (Stability of Altered Forest Ecosystems) community.
  #' 
  #' @param zenodoRecord, returned \code{list} object from an API call to the
  #'   Zenodo cloud storage database. Should contain information pertaining to
  #'   a Zenodo record (or records)
  #' @return \code{TRUE}/\code{FALSE} indicating whether the record is part of
  #'   the SAFE community
  #' @seealso \url{https://zenodo.org/communities/safe/},
  #'   \code{\link{zenodoRecordApiLookup}}, \code{\link{zenodoVersionsApiLookup}}
  
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
  #' Update the offline version cache for a given SAFE project
  #' 
  #' Takes the output from a call to the Zenodo 'versions' API and processes the
  #' returned \code{list} into a dataframe storing version details for all
  #' records associated with a given SAFE project (identified using the concept
  #' record ID number). Version fields include the DOI, record ID, creation
  #' date, access status ('open', 'embargoed', or 'closed'), and - in the case 
  #' of embargoed records - the open date (the date that the record will be
  #' publicly accessible) for all records associated with the concept ID. The
  #' constructed dataframe is stored at the specified directory \code{dir}.
  #' 
  #' @param versions, the full versions list from the Zenodo 'versions' API call
  #' @param dir, local SAFE cache directory
  #' @return a dataframe containing access status information for each version
  #'   of the specified SAFE project, which is saved at the \code{dir}.
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

processSafe <- function (id, dir, updateCache=TRUE) {
  #' Get record info for specified SAFE project ID from the Zenodo database
  #' 
  #' This function returns a \code{list} object from an API call to the Zenodo
  #' database using the supplied SAFE project ID number. The API call contains
  #' information required to download the requested SAFE dataset into the
  #' specified directory.
  #' 
  #' The function will automatically create a data folder at the directory
  #' \code{dir} and (optionally, \code{updateCache} boolean) updates an offline 
  #' cache of version information pertaining to the concept record ID associated
  #' with the requested record. Note that the version cache is automatically
  #' downloaded into the \code{dir} the first time a record is accessed.
  #' 
  #' \code{processSafe} additionally performs a series of checks on the record.
  #' Specifically, an error is raised in the event that the ID does not exist on
  #' Zenodo, or is not located in the SAFE community, or if the record is
  #' unavailable (i.e. is 'embargoed' or 'closed'). A warning message will be
  #' presented if a more recent version of the requested record is available for
  #' download.
  #' 
  #' @param id, SAFE project record ID number. This could be a concept record
  #'   ID or a DOI identifier for a specific record. In the event that a concept
  #'   record ID is passed, the function will attempt to return the most recent
  #'   'open' dataset associated with the project
  #' @param dir, directory into which data should be stored
  #' @param updateCache, \code{boolean}, should the version cache be updated?
  #' @return \code{list} object containing information from the Zenodo API call
  #' @seealso \link{\code{zenodoRecordApiLookup}}, \link{\code{updateSafeVersionCache}}
  
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
    if (!updateCache) {
      warning('No version cache found! Creating one now...')
      updateCache <- TRUE
    }
    dir.create(file.path(dir, conceptRecId))
  }
  
  # update and load the version cache
  if (updateCache) {
    message('Updating version cache for concept record ID ', conceptRecId, 
            '... ', appendLF=FALSE)
    updateSafeVersionCache(allVersions, dir=dir)
    message('complete!')
  }
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

downloadSafeFile <- function (zenodoRecord, dir) {
  #' Download SAFE project data file associated with the supplied record
  #' 
  #' 
  
  # check that it's a SAFE zenodo project?? maybe just trycatch the whole thing
  
  bucket <- jsonlite::fromJSON(zenodoRecord$links$bucket)
  fileName <- file.path(dir, basename(bucket$contents$links$self))
  download.file(bucket$contents$links$self, destfile=fileName)
}

getSafe <- function () {
  
  # could supply a list of project IDs so should be able to handle this
  # need to specify a SAFE directory for storing files
  
}
