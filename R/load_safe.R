load_safe_data <- function(record_id, worksheet) {

    #' Loads data from a SAFE dataset.
    #'
    #' This function returns a data frame containing the data from a data
    #' worksheet in a SAFE dataset. Note that SAFE dataset .xlsx files include
    #' the other (non-data) worksheets Summary, Taxa, Locations that contain
    #' metadata: see  \code{\link{get_taxa}}, \code{\link{get_locations}},
    #' \code{\link{add_taxa}} and \code{\link{add_locations}} for accessing
    #' and using this metadata.
    #'
    #' In particular, the large amount of data worksheet summary metadata is
    #' not attached as attributes to the data frame returned by this function.
    #' This is largely to avoid excessive output to the console during normal
    #' use of the data frame: an extended description of a worksheet can be
    #' displayed using \code{\link{show_worksheet}}.
    #'
    #' Currently, this function only loads data from SAFE formatted
    #' \code{.xlsx} files - data stored in external files is not yet
    #' handled.
    #'
    #' @param record_id A SAFE dataset record id
    #' @param worksheet The name of the worksheet to load
    #' @param x,object A \code{safedata} object.
    #' @param n The number of rows to show in the \code{print} method.
    #' @param \dots Further arguments to \code{str} and \code{print} methods.
    #' @return A data frame with the additional \code{safedata} class and
    #'    additional attribute data containing metadata for the data.
    #' @examples
    #'    set_example_safe_dir()
    #'    beetle_abund <- load_safe_data(1400562, "Ant-Psel")
    #'    str(beetle_abund)
    #'    # See also the show_worksheet function for further worksheet metadata
    #'    show_worksheet(beetle_abund)
    #'    unset_example_safe_dir()
    #' @export

    # validate the record id
    record_set <- validate_record_ids(record_id)

    # Logic of what to load.
    # a) If a record is provided, return that unless it is unavailable, in
    #    which case suggest an alternative.
    # b) If a concept is provided, load MRA if there is one.

    # Look for a local copy of the file. If it doesn't exist, download it
    # if possible
    index <- load_index()
    index_row <- subset(index, zenodo_record_id == record_set$record &
                               grepl(".xlsx$", filename))

    if (nrow(record_set) != 1) {
        stop("Requires a single valid record or concept id")
    } else if (is.na(record_set$record)) {
        # concept provided - we don't attempt to load local private copies here
        if (is.na(record_set$mra)) {
            stop("Concept ID provided: all records under embargo or restricted")
        } else {
            verbose_message("Concept ID provided: ",
                            "loading most recent available record")
            record_set <- validate_record_ids(record_set$mra)
        }
    } else {
        # record provided
        if (! record_set$available) {
            if (index_row$local_copy) {
                verbose_message("Loading data from private copy of ",
                                "embargoed or restricted data")
            } else if (is.na(record_set$mra)) {
                stop("Record ID provided: this and all other versions of this ",
                     "dataset concept are under embargo or restricted")
            } else {
                stop("Record ID provided: version is under embargo or ",
                     "restricted. Most recent available is ", record_set$mra)
            }
        } else {
            if (! (record_set$record ==  record_set$mra)) {
                verbose_message("Outdated record: the most recent available ",
                                "version is ", record_set$mra)
            }
        }
    }

    # Now get the metadata and check the target worksheet exists
    meta <- load_record_metadata(record_set)
    if (! worksheet %in% meta$dataworksheets$name) {
        stop("Data worksheet name not one of: ",
             paste(meta$dataworksheets$name, collapse = ", "))
    }

    # Download the data if needed
    if (! index_row$local_copy) {
        verbose_message("Downloading datafile: ", index_row$filename)
        downloaded <- download_safe_files(index_row$zenodo_record_id)
        if (! length(downloaded)) {
            stop("Data file unavailable")
        }
    }

    # Validate the local copy
    local_path <- file.path(getOption("safedata.dir"), index_row$path)
    local_md5 <- tools::md5sum(local_path)
    if (local_md5 != index_row$checksum) {
        stop("Local file has been modified - ",
             "do not edit files within the SAFE data directory")
    }

    # Now load the data - using readxl, openxlsx is also possible but seems
    # to be orphaned and has some date time handling issues. One issue with
    # readxl is it has field type guessing based on the Excel cell types of
    # the first N rows. Usually this is fine but can be tripped up (e.g. one
    # alphanumeric in a list of IDs or a sparsely populated field). So, we use
    # the field types to set column classes using a conversion map from safedata
    # types to readxl col_types: "skip", "guess", "logical", "numeric", "date",
    # "text" or "list". Datetime fields are left to the guessing mechanism
    # because they can be Excel dates or POSIX strings.
    dwksh <- meta$dataworksheets[meta$dataworksheets$name == worksheet, ]
    fields <- dwksh$fields[[1]]
    field_types <- tolower(fields$field_type)
    readxl_map <- c("date" = "guess", "datetime" = "guess", "time" = "guess",
                    "location" = "text", "latitude" = "numeric",
                    "longitude" = "numeric", "replicate" = "text",
                    "id" = "text", "categorical" = "text",
                    "ordered categorical" = "text", "numeric" = "numeric",
                    "taxa" = "text", "abundance" = "numeric",
                    "categorical trait" = "text", "numeric trait" = "numeric",
                    "categorical interaction" = "text",
                    "numeric interaction" = "numeric", "file" = "text",
                    "comments" = "text")

    # map the column types, including the first column of record numbers
    col_types <- c("numeric", readxl_map[match(field_types, names(readxl_map))])
    if (any(is.na(col_types))) {
        stop("Problem with column type specification. Contact developers")
    }

    # Read the data and then reduce to a data frame (not tibble) with no
    # row numbers. We want to check here if the field names are the same
    # as expected from the metadata. This is complicated by the fact that,
    # although field names in metadata should be syntactically valid R as
    # of safedata_validator 1.2.7, they weren't before that. So, enforce
    # make.names on both to avoid trivial mismatches. Turn off trim_ws in
    # read_xlsx because that occurs before name repair and makes names
    # with whitespace diverge between metadata and data names
    data <- readxl::read_xlsx(local_path, worksheet,
                              skip = dwksh$field_name_row - 1,
                              n_max = dwksh$n_data_row, na = "NA",
                              col_types = col_types,
                              trim_ws = FALSE,
                              .name_repair = ~ make.names(.x, unique = TRUE))
    class(data) <- "data.frame"
    data <- data[, -1]

    # Check field name matching.,
    if (! identical(make.names(fields$field_name, unique = TRUE), names(data))) {
        stop("Mismatch between data field names and local metadata")
    }

    # Now do field type conversions
     for (idx in seq_along(names(data))) {

        fld <- fields[idx, ]

        # Factors
        if (grepl("Categorical", fld$field_type)) {
            data[fld$field_name] <- as.factor(data[[fld$field_name]])

        }

        # Dates, Datetimes and Times
        if (fld$field_type %in% c("Date", "Datetime", "Time")) {

            values <- data[[fld$field_name]]
            # if they haven't been converted already, then the user has supplied
            # POSIX strings not Excel Date/Time
            if (! inherits(values, "POSIXt")) {
                values <- try(as.POSIXct(values, tryFormats =
                                         c("%Y-%m-%d %H:%M:%OS",
                                           "%Y-%m-%d %H:%M", "%Y-%m-%d",
                                           "%H:%M:%OS", "%H:%M")))
                if (inherits(values, "try-error")) {
                    stop("Failed to convert date/times in field ",
                         fld$field_name)
                }
            }
            # Use chron time - could use chron date + chron but they provide a
            # ugly pile of attributes in the data frame display and have to be
            # beaten with a stick to stop them using month/day/year formats.
            if (fld$field_type == "Time") {
                values <- chron::times(format(values, "%H:%M:%S"))
            }
            data[fld$field_name] <- values
        }
    }

    # Design notes on methods: we want safedata to behave as much as possible
    # like a data frame. The attributes are used to record provenance and the
    # safedata class is primarily used to allow loaded data frames to be passed
    # to the show_* metadata functions. With S3 generics, the set of classes is
    # used in order, so we only need to provide safedata S3 methods where we
    # want to modify the default dataframe methods - this is only str, where
    # hiding the attributes and displaying that information at top is
    # aesthetically nicer.

    class(data) <- c("safedata", "data.frame")
    dwksh <- as.list(dwksh)
    dwksh$safe_record_set <- record_set
    attr(data, "metadata") <- dwksh
    return(data)
}


str.safedata <- function(object, ...) {

    #' @describeIn load_safe_data Display structure of a safedata data frame
    #' @export

    object_attr <- attr(object, "metadata")
    msg <- "SAFE dataset\nConcept: %i; Record %i; Worksheet: %s\n"
    with(object_attr, cat(sprintf(msg, safe_record_set$concept,
                                  safe_record_set$record, name)))

    # reduce the safedata object to a simple data frame and pass back
    # to str(x, ...)
    attr(object, "metadata") <- NULL
    class(object) <- "data.frame"
    invisible(str(object, ...))

}

print.safedata <- function(x, n = 10, ...) {

    #' @describeIn load_safe_data Print safedata data frame
    #' @export

    x_attr <- attr(x, "metadata")
    msg <- "SAFE dataset:\nConcept: %i; Record %i; Worksheet: %s\n"
    with(x_attr, cat(sprintf(msg, safe_record_set$concept,
                             safe_record_set$record, name)))

    if (inherits(x, "sf")) {
        options(sf_max_print = n)
        NextMethod()
    } else if (inherits(x, "data.frame")) {
        class(x) <- "data.frame"
        cat(sprintf("First %i rows:\n", n))
        print(head(x, n = n))
    }

    return(invisible(x))
}

# Access notes: There are two routes to files within Zenodo - via the API
# and via the website URL. For example, these two URLs get the same file:
#
# https://zenodo.org/api/files/2edc1bf2-e84e-40be-882d-08ce476c3bcb/SAFE_Gazetteer_metadata_v3.xlsx
# https://www.zenodo.org/record/3906082/files/SAFE_Gazetteer_metadata_v3.xlsx
#
# ** API URLs **
# The API link requires that hex 'bucket' id - and these are not stable
# so need to be retrieved when a user requests a download. The record,
# details are available from an API call:
#,
# https://zenodo.org/api/records/3906082
#,
# If the record is _open_ then the JSON response contains a files array
# giving the API download path. However, if the record is not open, that
# files array is not present in the response.,
#
# An access token can be passed to the records API call, which will then
# report file URLs for any record in the community. The same token can then
# be used with the files API to allow any file to be downloaded. However,
# these are developer tokens and provide root access so are not for public
# use.
#
# ** WWW URLs **
# The safedata index actually contains all the details needed to recreate a
# URL for any file - only the record number and filename are needed. The
# filenames are not typically known for embargoed and restricted files,
# but safedata stores them. However, the file URL will raise a 404 error
# unless the file is open access.,
#,
# There is a special case - if a user requests access to restricted,
# dataset, then they get a token that will allow that file to be downloaded
# from the appropriate WWW URL (_not_ via the API URL):
#
# https://sandbox.zenodo.org/record/315677/files/test.xlsx?token=eyJhbGc...
#
# There is currently no such mechanism for embargoed data - you just have,
# to wait it out or contact the authors.
#
# The download_safe_files function uses the WWW URLs to support restricted
# file tokens and to remove the need for an API intermediate call. A
# developer version could use the API URLs as a common framework to download
# everything using an API token.

download_safe_files <- function(record_ids, confirm = TRUE, xlsx_only = TRUE,
                                download_metadata = TRUE, refresh = FALSE,
                                token = NULL) {

    #' Download SAFE dataset files
    #'
    #' This downloads files associated with SAFE datasets, either all of the
    #' files included in a set of records (\code{xlsx_only = FALSE}) or just
    #' the core .xlsx files (\code{xlsx_only = FALSE}), and stores them in the
    #' SAFE data directory. See \code{\link{insert_dataset}} for details on
    #' using embargoed or restricted datasets.
    #'
    #' By default, the function will also download the dataset metadata. This
    #' information is required by many of the functions in the package but users
    #' can turn off automatic metadata download.
    #'
    #' @section Warning:
    #' Using \code{refresh = TRUE} will \strong{overwrite locally modified
    #' files} and replace them with the versions of record from Zenodo.
    #'
    #' @param record_ids A vector of SAFE dataset record ids or a
    #'    \code{\link{safe_record_set}} object.
    #' @param confirm Requires the user to confirm before download (logical)
    #' @param xlsx_only Should all files be downloaded or just the core .xslx
    #'    file (logical)
    #' @param download_metadata Should the metadata record for the file be
    #'    downloaded (logical)
    #' @param refresh Should the function check if local copies have been
    #'    modified and download fresh copies. This is useful if the local
    #'    copies have unintentionally been modified but note the warning above.
    #' @param token An access token for restricted datasets. These tokens are,
    #'    requested through the Zenodo page for a restricted dataset and are,
    #'    long alphanumeric strings. If you are providing a token, you should
    #'    only provide the record id for that dataset.
    #' @return Invisibly, a vector of paths for successfully downloaded files.
    #' @examples
    #'    \donttest{
    #'        set_example_safe_dir()
    #'        recs <- validate_record_ids(c(3247631, 3266827, 3266821))
    #'        download_safe_files(recs, confirm = FALSE)
    #'        unset_example_safe_dir()
    #'    }
    #'    \dontrun{
    #'        # This example requires a private token
    #'        download_safe_files(1237730, confirm = FALSE,
    #'                            token="longStringFromZenodo")
    #'    }
    #' @export

    # validate the record ids
    record_set <- validate_record_ids(record_ids)

    records_to_get <- record_set$record[! is.na(record_set$record)]

    if (! length(records_to_get)) {
        verbose_message("No valid record ids provided")
        return(invisible())
    } else if (! is.null(token) & length(records_to_get) > 1) {
        verbose_message("When using an access token, please download ",
                        "the single record")
        return(invisible())
    }

    # Get the target files
    index <- load_index()
    safedir <- get_data_dir()

    # Get the set of files
    if (xlsx_only) {
        targets <- subset(index, zenodo_record_id %in% records_to_get &
                                 grepl(".xlsx$", filename))
    } else {
        targets <- subset(index, zenodo_record_id %in% records_to_get)
    }

    # See what is stored locally
    targets$local_path <- file.path(safedir, targets$path)
    targets$local_exists <- file.exists(targets$local_path)

    # Check which files are already local and optionally which have bad MD5 sums
    if (refresh) {
        targets$refresh <- targets$checksum != tools::md5sum(targets$local_path)
    } else {
        targets$refresh <- FALSE
    }

    # Create the confirmation message
    msg <- paste0("%i files requested from %i records\n",
                  " - %i local (%s)\n",
                  " - %i embargoed or restricted (%s)\n",
                  " - %i to download (%s)")

    local <- subset(targets, (! refresh) & local_exists)
    unavail <- subset(targets, ! available)
    to_download <- subset(targets, (refresh | (! local_exists)) & available)

    size_to_human <- function(size) {
        return(format(structure(size, class = "object_size"), units = "auto"))
    }

    msg <- sprintf(msg, nrow(targets), length(unique(targets$zenodo_record_id)),
                   nrow(local), size_to_human(sum(local$filesize)),
                   nrow(unavail), size_to_human(sum(unavail$filesize)),
                   nrow(to_download), size_to_human(sum(to_download$filesize)))

    if (confirm) {
        # Don't mute the message if the function is called to report this!
        confirm_response <- utils::menu(c("Yes", "No"), title = msg)
        if (confirm_response != 1) {
            message("Aborting download")
            return(invisible())
        }
    } else {
        verbose_message(msg)
    }

    # download metadata if requested
    if (download_metadata) {
        fetch_record_metadata(record_set)
    }

    # split by records
    files_by_record <- split(targets, targets$zenodo_record_id)
    downloaded <- character()

    for (these_files in files_by_record) {

        current_record <- these_files$zenodo_record_id[1]

        if (! is.null(token) & all(these_files$dataset_access == "restricted")) {
            verbose_message("Using token to access restricted record")
        } else if (! these_files$available[1]) {
            msg <- "%i files for record %i: under embargo or restricted"
            verbose_message(sprintf(msg, nrow(these_files), current_record))
            next
        }

        verbose_message(sprintf("%i files for record %i: %i to download",
                                nrow(these_files), current_record,
                                sum((! these_files$local_exists) |
                                    these_files$refresh)))

        these_files <- subset(these_files, (! local_exists) | refresh)

        if (nrow(these_files)) {
            # create download urls
            these_files$public_url <- sprintf("https://www.zenodo.org/record/%s/files/%s",
                                              these_files$zenodo_record_id,
                                              these_files$filename)

            # Handle token if provided
            if (! is.null(token)) {
                these_files$public_url <- paste0(these_files$public_url, "?token=", token)
            }

            # Now download the required files
            for (row_idx in seq_along(these_files$filename)) {

                this_file <- these_files[row_idx, ]

                # Look to see if the target directory exists.
                if (! file.exists(dirname(this_file$local_path))) {
                    dir.create(dirname(this_file$local_path),
                               recursive = TRUE)
                }

                # Download the target file to the directory
                result <-  try(
                    curl::curl_download(this_file$public_url,
                                        dest = this_file$local_path),
                               silent = TRUE)

                if (inherits(result, "try-error")) {
                    if (this_file$local_exists) {
                        verbose_message(" - Failed to refresh: ",
                                        this_file$filename)
                    } else {
                        verbose_message(" - Failed to download: ",
                                        this_file$filename)
                    }
                } else {
                    if (this_file$local_exists) {
                        verbose_message(" - Refreshed: ",
                                        this_file$filename)
                    } else {
                        verbose_message(" - Downloaded: ",
                                        this_file$filename)
                    }
                    downloaded <- c(downloaded, this_file$local_path)
                }
            }
        }
    }
    return(invisible(downloaded))
}


insert_dataset <- function(record_id, files) {

    #' Inserts local copies of files from a dataset into a SAFE data directory
    #'
    #' If files are embargoed or restricted, then users may request the
    #' datafiles from the authors. This function allows provided files
    #' to be incorporated into a SAFE data directory, so that they will
    #' then work seamlessly alongside openly available data.
    #'
    #' @param record_id A SAFE dataset record id
    #' @param files A vector of files to insert into the data directory
    #' @return NULL
    #' @examples
    #'    set_example_safe_dir()
    #'    files <- system.file("safedata_example_dir",
    #'                         "template_ClareWfunctiondata.xlsx",
    #'                         package = "safedata")
    #'    insert_dataset(1237719, files)
    #'    dat <- load_safe_data(1237719, "Data")
    #'    str(dat)
    #'    unset_example_safe_dir()
    #' @export

    record_set <- validate_record_ids(record_id)
    if (nrow(record_set) != 1) {
        stop("record_id must identify a single record")
    } else if (is.na(record_set$record)) {
        stop("record_id cannot be a concept record id")
    }

    # Get the list of possible files for this record
    index <- load_index()
    record_files <- subset(index, zenodo_record_id == record_set$record)

    # Validate incoming files
    local_md5 <- tools::md5sum(files)

    # Do the provided files actually exist
    missing_files <- is.na(local_md5)
    if (any(missing_files)) {
        stop("Files not found: ", paste0(files[missing_files], collapse = ","))
    }

    # Add index data on to the local files
    local_files <- data.frame(local_path = files, filename = basename(files),
                              local_md5 = local_md5, stringsAsFactors = FALSE)
    local_files  <- merge(local_files, record_files,
                          by = "filename", all.x = TRUE)

    # Are the provided filenames part of the record
    unknown_files <- is.na(local_files$checksum)
    if (any(unknown_files)) {
        stop("Local files not found in record metadata: ",
             paste0(local_files$filename[unknown_files], collapse = ","))
    }

    # Are they the same files - compare checksums
    non_matching_checksums <- local_files$local_md5 != local_files$checksum
    if (any(non_matching_checksums)) {
        stop("Local file checksums do not match record metadata: ",
             paste0(local_files$filename[non_matching_checksums],
                    collapse = ","))
    }

    # Now we can insert them - skipping files already present
    local_files$current_safe_dir_path <- file.path(getOption("safedata.dir"),
                                                   local_files$path)
    local_files$local_copy <- file.exists(local_files$current_safe_dir_path)

    if (any(local_files$local_copy)) {
        verbose_message("Skipping files already present: ",
                        paste0(local_files$filename[local_files$local_copy],
                               collapse = ","))
        local_files <- subset(local_files, ! local_copy)
    }

    if (nrow(local_files)) {
        verbose_message("Inserting files: ",
                        paste0(local_files$filename, collapse = ","))
        copy_success <- try({
            dir.create(dirname(local_files$current_safe_dir_path[1]),
                       recursive = TRUE)
            with(local_files, file.copy(local_path, current_safe_dir_path))
            })
        if (inherits(copy_success, "try-error")) {
            stop("Failed to insert files:",
                 paste0(local_files$filename[! copy_success], collapse = ","))
        } else {
            # update the index
            index$local_copy[index$checksum %in% local_files$checksum] <- TRUE
            assign("index", index, safedata_env)
        }
    }

    return()
}
