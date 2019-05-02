readTransposedXlsx <- function (file, sheetName, ...) {
  #' Read a transposed .xlsx file into a dataframe
  #' 
  #' Provides capability to read .xlsx files that are transposed (i.e. 
  #' organised with headers in rows and data across columns) into a standard R
  #' dataframe. This function preserves data types.
  #' 
  #' @param path The path to the .xlsx file to be opened
  #' @param sheetName The name of the worksheet to be imported
  #' @param ... Optional arguments to be passed to \code{read_xlsx}
  #' @return The reformatted .xlsx with headers as columns and data in rows
  #' 
  #' @note Dependency on package \code{ReadXL}
  
  df <- suppressMessages(read_xlsx(file, sheet=sheetName, col_names=FALSE, ...))
  dfT <- as.data.frame(t(df[-1]), stringsAsFactors=FALSE)
  names(dfT) <- t(df[,1])
  dfT <- as.data.frame(lapply(dfT, type.convert))
  
  return(dfT)
}

simpleCap <- function (str) {
  #' Capitalize the first letter of each word in a string
  #' 
  #' This function takes a string, capitalizes the first letter of each word
  #' within it, removes all whitespace, and combines the result into a single
  #' word, returning the result.
  #' 
  #' @param str The string to be capitalized
  #' @return The original string pasted together with all whitespace removed and
  #'   the first letter of each word capitalized
  #' @examples simpleCap("The quick brown fox jumps over the lazy dog")
  
  x <- strsplit(str, " ")[[1]]
  y <- paste(toupper(substring(x, 1, 1)), substring(x, 2), sep="", collapse=" ")
  return(y)
}

processSummary <- function (df) {
  #' Process summary "meta" information for SAFE project data
  #' 
  #' Initiates a SAFE data object from the summary sheet provided. This function
  #' reads and assigns variables from the provided summary dataframe (a
  #' transposed "Summary" worksheet from a SAFE project submission file) into a
  #' new SAFE \code{list} object.
  #' 
  #' @param dataframe Dataframe containing summary information. This must be a 
  #'   "Summary" worksheet from a SAFE project submission file
  #' @return SAFE List object containing summary information for the project
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/summary/}
  #'   for information on the SAFE summary worksheet
  
  safeObj <- list()
  safeObj$projectID <- df$SAFE.Project.ID[1]
  safeObj$title <- as.character(df$Title[1])
  safeObj$workSheets <- gsub(" ", "", lapply(subset(
    as.character(df$Worksheet.name), !is.na(df$Worksheet.name)), simpleCap))
  for (i in 1:length(safeObj$workSheets)) {
    safeObj[[safeObj$workSheets[i]]] <- list()
  }
  safeObj$startDate <- as.Date.numeric(df$Start.Date[1], origin="1899-12-30")
  safeObj$endDate <- as.Date.numeric(df$End.Date[1], origin="1899-12-30")
  
  return(safeObj)
}

printSummary <- function (safeObj) {
  #' Prints summary information for SAFE object
  #' 
  #' Prints a summary of metadata for the given SAFE project object to the
  #' command line. Includes the project title, ID number, start and end dates,
  #' and data worksheet names.
  #' 
  #' @param safeObj Existing SAFE data object with meta information added
  #' @seelso \code{\link{processSummary}}
  
  cat("Project name:", safeObj$title, "\n")
  cat("Project ID:", safeObj$projectID, "\n")
  cat("Dates:", paste(safeObj$startDate), "to", paste(safeObj$endDate), "\n")
  cat("Contains", length(safeObj$workSheets), "worksheets:", "\n")
  cat("  ", paste(safeObj$workSheets, collapse=", "))
}

processTaxa <- function (file, safeObj) {
  #' Add taxa information to SAFE data object
  #'
  #' This function adds a new dataframe to an existing SAFE object containing
  #' all taxonomic information for the submitted dataset. Note that when the
  #' data do not contain any taxa this table defaults to \code{NA}. When
  #' provided, this dataframe is used to compile a complete taxonomic heirarchy 
  #' for all taxa identified within the dataset.
  #' 
  #' @param file Complete path to the SAFE project file
  #' @param safeObj An existing SAFE data object
  #' @return A modified SAFE object with a Taxa dataframe
  #' @note Not all SAFE project submissions contain the Taxa worksheet (for
  #'   example if the data do not contain taxa). In this case the SAFE Taxa
  #'   object defaults to \code{NA}.
  #' @seealso \code{\link{buildTaxonHeirarchy}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/taxa/}
  #'   for information on the Taxa worksheet
  
  safeObj$Taxa <- tryCatch(
    {
      as.data.frame(read_xlsx(file, "Taxa", col_names=TRUE))
    },
    error = function(cond) {
      message("SAFE import note: No Taxa datasheet supplied")
      return(NA)
    }
  )
  return(safeObj)
}

buildTaxonHeirarchy <- function (safeObj) {
  return(safeObj)
}

processLocations <- function (file, safeObj) {
  #' Adds location information to SAFE data object
  #' 
  #' Processes the \code{Locations} worksheet in the SAFE project file and adds
  #' it as a dataframe to the existing SAFE data object.
  #' 
  #' @param file Complete path to the SAFE object file
  #' @param safeObj An existing SAFE data object
  #' @return A modified SAFE object with a Locations dataframe
  #' @note Although unusual, not all SAFE project submissions will contain a
  #'   Locations worksheet. Examples of this include projects that present only
  #'   laboratory data (that do not have the requirement of specifying where
  #'   field samples came from), or for inconsistent data collection locations
  #'   (e.g. tracking animal movements). In the case of the latter GPS data
  #'   should be provided separately for each observation.
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/locations/}
  #'   for details on the Locations worksheet
  
  safeObj$Locations <- tryCatch(
    {
      as.data.frame(read_xlsx(file, "Locations", col_names=TRUE))
    },
    error = function(cond) {
      message("SAFE import note: No Locations datasheet supplied")
      return(NA)
    }
  )
  return(safeObj)
}

processData <- function (file, safeObj) {
  #' Add data worksheets to SAFE project object
  #' 
  #' This function processes data-containing worksheets for the given SAFE
  #' project file and adds them to an existing SAFE data object. This function
  #' works by creating a new \code{list} within the SAFE object for each
  #' worksheet found within the SAFE project file. These lists are named
  #' according to their names in the original SAFE project file. Each includes a
  #' \code{data} and \code{attributes} field containing, respectively, the data
  #' and header information within the given worksheet. Note that this function
  #' attempts to read all data types according to the \code{field_type}
  #' provided in the SAFE data table.
  #' 
  #' @param file Path to the SAFE project file. This is assumed to be .xlsx
  #' @param safeObj An existing SAFE data object
  #' @return A modified SAFE data object containing data worksheets 
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/}
  #'   for information on SAFE data worksheets
  
  sheets <- excel_sheets(file)
  sheetsNormed <- gsub(" ", "", lapply(sheets, simpleCap))
  
  for (i in 1:length(safeObj$workSheets)) {
    idx <- which.max(sheetsNormed==safeObj$workSheets[i])
    
    # get line index where actual data begins (at "field_name" header)
    fullData <- as.data.frame(suppressMessages(
      read_xlsx(file, sheets[idx], col_names=FALSE, na=c("", "NA"))))
    firstDataRow <- which.max(fullData[,1] == "field_name")
    
    # extract meta information from header lines
    headerInfo <- readTransposedXlsx(fPath, sheets[idx], na=c("", "NA"),
                                     n_max=firstDataRow-1)
    fieldTypes <- c("numeric", sapply(headerInfo$field_type, getDataClass))
    safeObj[[safeObj$workSheets[i]]]$attributes <- headerInfo
    
    # store actual data (without header info) "en masse"
    data <- as.data.frame(suppressMessages(
      read_xlsx(fPath, sheets[idx], col_names=TRUE, col_types=fieldTypes,
                skip=firstDataRow-1, na=c("", "NA"))))

    # convert categorical data types
    categoricals <- c(FALSE, sapply(headerInfo$field_type, isCategorical))
    factorCols <- names(data)[categoricals]
    data[factorCols] <- lapply(data[factorCols], factor)
    safeObj[[safeObj$workSheets[i]]]$data <- data
  }
  
  return(safeObj)
}

getDataClass <- function (safeType) {
  #' Get the R data type from the SAFE \code{field_type} variable
  #' 
  #' Takes a SAFE \code{field_type} variable and returns the appropriate ReadXl
  #' data type. This function is handy for ensuring SAFE worksheet data tables
  #' are correctly imported by ReadXl (i.e. that format types are consistent).
  #' If the data type is not identifiable then this function will return a value
  #' of "guess".
  
  #' @param safeType The SAFE \code{field_type} variable
  #' @return The corresponding readxl data type
  #' @seealso \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/}
  #'   for accepted SAFE data field types and 
  #'   \url{https://cran.r-project.org/web/packages/readxl/} for ReadXl data types
  
  typeDate <- c("Date","Datetime", "Time")
  typeText <- c("Location", "ID", "Taxa", "Replicate", "Abundance")
  typeNum <- c("Latitude", "Longitude", "Numeric")
  typeFac <- c("Categorical", "Ordered Categorical", "Categorical Trait",
               "Numeric Trait", "Categorical Interaction", "Numeric Interaction")
  
  if (safeType %in% typeDate) {
    return("date")
  } else if (safeType %in% typeText) {
    return("text")
  } else if (safeType %in% typeNum) {
    return("numeric")
  } else if (safeType %in% typeFac) {
    return("text")
  } else {
    return("guess")
  }
}

isCategorical <- function (safeType) {
  #' Check if the SAFE data type is of R type "categorical"/"factor"
  #' 
  #' Takes a SAFE \code{field_type} and checks whether it is of R type
  #' "categorical"/"factor". This is used to convert SAFE data variables
  #' after import.
  #' 
  #' @param safeType The SAFE \code{field_type} variable
  #' @return \code{TRUE} if given variable is a categorical, \code{FALSE} if not
  #' @seealso \code{\link{getDataClass}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/data/}
  #'   for differernt SAFE data types
  
  if (safeType %in% c("Categorical", "Ordered Categorical", "Categorical Trait",
                      "Numeric Trait", "Categorical Interaction",
                      "Numeric Interaction")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

getTaxonHeirarchy <- function(taxaRow, ...) {
  #' Return taxonomic heirarchy for a SAFE taxonomic entry
  #' 
  #' This function attempts to perform a search of the GBIF database for the
  #' named Taxon/Parent types in the SAFE project Taxa worksheet. The purpose
  #' of this is to construct a taxonomic tree for all taxa reported in the
  #' dataset(s) to facilitate data searching/subsetting.
  #' 
  #' @param taxaRow A row from a SAFE object Taxa worksheet
  #' @param ... Optional arguments to pass to \code{name_backbone}
  #' 
  #' @seealso \code{\link[rgbif]{name_backbone}},
  #'   \code{\link[rgbif]{name_usage}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/taxa/},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/safe_dataset_checker/gbif_validation/}
  
  # *NOTE* I'm not sure if this is the most efficient way to run this!
  
  if (!is.na(taxaRow[["Parent name"]])) {
    # The Parent Name has been provided. This means one of 3 things:
    # 1. The Taxon type is a morphospecies or functional group
    # 2. The Taxon name:type is unrecognized
    # 3. The Taxon is from a less common taxonomic level
    # In any case, taxonomic look-ups need to be done at the Parent level
    level = "Parent"
  } else {
    # No Parent Name given so look-ups can be done on the provided Taxon
    level = "Taxon"
  }
  
  # Taxonomic look-ups then follow a multi-step process
  # 1. strict search on the name backbone
  nameBackbone <- name_backbone(name=taxaRow[[paste(level, "name")]],
                                rank=taxaRow[[paste(level, "type")]], 
                                strict=TRUE, verbose=FALSE, ...)
  
  # 2. if this search fails there are a few options
  if (nameBackbone$matchType == "NONE") {
    # (i) attempt to match on Taxon/Parent ID (if this has been provided)
    # this is the case when there are multiple entries for the given name
    # first get all alternatives of the name using a strict search
    if (!is.na(taxaRow[[paste(level, "ID")]])) {
      altOpts <- name_backbone(name=taxaRow[[paste(level, "name")]],
                               rank=taxaRow[[paste(level, "type")]], 
                               strict=TRUE, verbose=TRUE, ...)$alternatives
      # then match the ID against the GBIF entry within the alternatives
      nameBackbone <- altOpts[altOpts$usageKey==taxaRow[[paste(level, "ID")]],]
    } else {
      # (ii) when no Taxon/Parent ID is given, perform non-strict GBIF search
      altOpts <- name_backbone(name=taxaRow[[paste(level, "name")]],
                               rank=taxaRow[[paste(level, "type")]], 
                               strict=FALSE, verbose=TRUE, ...)
      if (altOpts$data$matchType != "NONE") {
        # take the top-matched entry, which may be a higher-rank
        nameBackbone <- altOpts$data
      } else {
        # in rare cases this still fails, so take the alternative GBIF search
        # option that has the highest confidence value
        nameBackbone <- altOpts$alternatives[
          which.max(altOpts$alternatives$confidence),]
      }
    }
  }
  return(nameBackbone)
}

safeWrapper <- function (file) {
  #' Wrapper to open and process SAFE data file
  
  summary <- readTransposedXlsx(file, sheetName="Summary")
  safeObj <- processSummary(summary)
  safeObj <- processTaxa(file, safeObj)
  safeObj <- processLocations(file, safeObj)
  safeObj <- processData(file, safeObj)
  
  return(safeObj)
}