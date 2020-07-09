#' Index file memory cache
#'
#' The \code{safedata_env} environment within the package namespace is
#' used to cache a copy of the dataset index, gazetteer and location aliases
#' rather than needing to repeatedly read these from file. This environment
#' is accessed by internal functions and is not intended for the end user.
#'
#' @keywords internal
#' @aliases index

safedata_env <- new.env(parent = emptyenv())

set_safe_dir <- function(safedir, update = TRUE, create = FALSE,
                         url = "https://www.safeproject.net") {

    #' Set the local SAFE data directory
    #'
    #' This function sets the local directory used to store SAFE dataset
    #' files along with record and index metadata. The function can also
    #' initialise a new data directory, downloading the required index
    #' files. By default, it will update indices if needed and will
    #' validate the directory contents. Once set, the location of the
    #' directory is stored in options("safedata.dir"). The \code{url}
    #' argument specifies a URL to a website that exposes the SAFE data
    #' API.
    #'
    #' The safedata package uses a data directory to store local copies of
    #' dataset files along with index files. Files for a dataset record are
    #' stored in subdirectories using the zenodo concept id for the record
    #' and then record id: \code{3342494/3342495}, for example. In addition
    #' to data files from these records, these folders can also contain a
    #' JSON file containing record metadata (e.g. \code{3342495.json}): this
    #' is a structured version of the summary information shown on the
    #' Zenodo page.
    #'
    #' The root of the data directory also holds three index files:
    #' \describe{
    #'   \item{\code{index.json}}{, containing a full list of the
    #'         files and dataset records available in the SAFE data
    #'         repository;}
    #'   \item{\code{gazetteer.geojson}}{, containing the official list of
    #'         known sampling locations and GIS data; and}
    #'   \item{\code{location_aliases.csv}}{, a list of alternative names
    #'         permitted for some locations.}
    #' }
    #'
    #' If \code{create = TRUE}, the function will try to create the named
    #' directory and populate it with the three index files. This requires
    #' an internet connection.
    #'
    #' By default, the function also needs an internet connection to check
    #' for updates to the three index files. Updating can be turned off for
    #' offline use.
    #'
    #' The default behaviour is also to validate the directory structure. The
    #' function will warn when files other than those found in datasets are
    #' present within the data structure and when any dataset files that are
    #' present have been modified. Although this can be turned off, it is
    #' not recommended to modify or add files within a SAFE data directory.
    #'
    #' @param safedir A path to the directory to set as the SAFE data
    #'    directory (str).
    #' @param update Should the local dataset index be updated (logical)?
    #' @param create Should a new data directory be created at the provided
    #'    path (logical)?
    #' @param url A URL providing the SAFE Data API, defaulting to the SAFE
    #'    Project's own URL.
    #' @return NULL
    #' @export

    # path expand to make paths work in md5sum, which fails with ~/.
    safedir <- path.expand(safedir)

    index_path <- file.path(safedir, "index.json")
    gazetteer_path <- file.path(safedir, "gazetteer.geojson")
    location_aliases_path <- file.path(safedir, "location_aliases.csv")
    url_path <-  file.path(safedir, "url.json")

    # Handle create first
    if (create) {

        # We don't want a existing directory that might have stuff in it
        if (dir.exists(safedir)) {
            stop("Directory already exists")
        }

        # create the directory and set it as safe data directory
        dir.create(safedir)
        options(safedata.dir = safedir)

        # Save the URL and set it
        jsonlite::write_json(list(url = url), url_path)
        options(safedata.url = url)

        # download the index file, then cache it
        download_index()
        index <- load_index()

        # download the gazetteer and location aliases
        download_gazetteer()
        download_location_aliases()

        verbose_message("Safe data directory created")
        return(invisible())
    }

    # Now validate an existing directory

    if (! dir.exists(safedir)) {
        stop("Directory not found.")
    }

    if (! file.exists(url_path)) {
        stop("API URL not found.")
    }

    if (! file.exists(index_path)) {
        stop("Dataset index not found.")
    }

    if (! file.exists(gazetteer_path)) {
        stop("Gazetteer not found.")
    }

    if (! file.exists(location_aliases_path)) {
        stop("Location aliases not found.")
    }

    # Set the data directory and then load the index file and URL. Only load
    # the gazetteer and location aliases if the user starts using locations.
    options(safedata.dir = safedir)

    url <- jsonlite::read_json(url_path)$url
    options(safedata.url = url)

    index <- load_index()

    # Look for updates
    if (update) {

        verbose_message("Checking for updates")

        # Get the current index hashes from the SAFE Data API and
        # then check each of the three index files
        index_hashes <- jsonlite::fromJSON(paste0(url, "/api/index_hashes"))

        # Check the index
        if (tools::md5sum(index_path) != index_hashes$index) {
            verbose_message(" - Updating index")
            # reload the index into the cache and get it
            download_index()
            index <- load_index(reload = TRUE)
        } else {
            verbose_message(" - Index up to date")
        }

        # Check the gazetteer
        if (tools::md5sum(gazetteer_path) != index_hashes$gazetteer) {
            verbose_message(" - Updating gazetteer")
            download_gazetteer()
        } else {
            verbose_message(" - Gazetteer up to date")
        }

        # Check the location aliases
        loc_alias_hash <- tools::md5sum(location_aliases_path)
        if (loc_alias_hash != index_hashes$location_aliases) {
            verbose_message(" - Updating location aliases")
            download_location_aliases()
        } else {
            verbose_message(" - Location aliases up to date")
        }
    }

    # Validate the directory contents
    verbose_message("Validating directory")

    # Run a check on directory structure
    local_files <- dir(safedir, recursive = TRUE)

    # Exclude the three index files and local metadata json files
    index_files <- c(basename(index_path), basename(gazetteer_path),
                     basename(location_aliases_path), basename(url_path))
    json_files <- grepl("[0-9]+/[0-9]+/[0-9]+.json$", local_files)
    metadata_json <- local_files[json_files]
    local_files <- setdiff(local_files, c(index_files, metadata_json))

    local_unexpected <- setdiff(local_files, index$path)
    if (length(local_unexpected)) {
        warning("SAFE data directory contains unexpected files: ",
                paste(local_unexpected, collapse = ", "))
    }

    # Run a check on file modification
    local_expected <- subset(index, file.exists(file.path(safedir, index$path)))
    local_expected$md5 <- tools::md5sum(file.path(safedir, local_expected$path))
    local_expected$altered <- with(local_expected, md5 != checksum)

    if (sum(local_expected$altered)) {
        warning("Local copies of dataset files have been modified",
                paste(local_expected$filename[local_expected$altered],
                      collapse = ", "))
    }

    # Update index to note which files have unaltered local copies (this
    # could include embargoed and restricted datasets, so this is used as a
    # flag to note which can be loaded from private local copies).
    local_unaltered <- local_expected$checksum[! local_expected$altered]
    index$local_copy <- index$checksum %in% local_unaltered
    assign("index", index, safedata_env)

    return(invisible())
}


download_index <- function() {

    #' Downloads the current dataset index
    #'
    #' This function downloads the dataset index from the SAFE data
    #' API (e.g. \url{https://www.safeproject.net/api/index}) and saves
    #' it in the root SAFE data directory
    #'
    #' @return NULL
    #' @keywords internal

    safedir <- get_data_dir()
    path <- file.path(safedir, "index.json")
    url <- getOption("safedata.url")
    api <- paste0(url, "/api/index")
    result <- try(curl::curl_download(api, path), silent = TRUE)

    if (inherits(result, "try-error")) {
        stop("Failed to download index")
    }

    return(NULL)

}


download_gazetteer <- function() {

    #' Downloads the current SAFE gazetteer
    #'
    #' This function downloads the gazetteer from the SAFE data
    #' gazetteer API (e.g. \url{https://www.safeproject.net/api/gazetteer})
    #' and saves it in the root of the safe data directory.
    #'
    #' @return NULL
    #' @keywords internal

    safedir <- get_data_dir()
    path <- file.path(safedir, "gazetteer.geojson")
    url <- getOption("safedata.url")
    api <- paste0(url, "/api/gazetteer")
    result <- try(curl::curl_download(api, path), silent = TRUE)

    if (inherits(result, "try-error")) {
        stop("Failed to download gazetteer")
    }

    return(NULL)
}


download_location_aliases <- function() {

    #' Downloads the current SAFE location aliases
    #'
    #' This function downloads the location aliases from the SAFE data
    #' location aliases API (for example,
    #' \url{https://www.safeproject.net/api/location_aliases})
    #' and saves it in the root of the safe data directory.
    #'
    #' @return NULL
    #' @keywords internal

    safedir <- get_data_dir()
    path <- file.path(safedir, "location_aliases.csv")
    url <- getOption("safedata.url")
    api <- paste0(url, "/api/location_aliases")
    result <- try(curl::curl_download(api, path), silent = TRUE)

    if (inherits(result, "try-error")) {
        stop("Failed to download location aliases")
    }

    return(NULL)
}


load_index <- function(reload = FALSE) {

    #' Load and cache the dataset index
    #'
    #' This function loads the dataset record index from the JSON file in
    #' the SAFE data directory and sets which datasets are currently available
    #' and which records are the most recent available under a given concept.
    #'
    #' When any of the three index files (\code{index.json},
    #' \code{gazetteer.geojson} and \code{location_aliases.csv}) are loaded,
    #' the file contents are cached in memory to reduce load times. The cache
    #' is within an environment that is not exported in the package namespace
    #' and is not intended to be user accessible.
    #'
    #' @param reload Force a refresh of the cached index from the file.
    #' @return A data frame containing the dataset index details.
    #' @seealso \code{\link{load_location_aliases}},
    #'     \code{\link{load_gazetteer}}
    #' @keywords internal

    if (exists("index", safedata_env) & ! reload) {
        index <- get("index", safedata_env)
    } else {
        safedir <- get_data_dir()
        index_path <- file.path(safedir, "index.json")

        index <- jsonlite::fromJSON(index_path)
        index <- index$entries

        # format the datetime classes
        index$publication_date <- as.POSIXct(index$publication_date)
        index$dataset_embargo <- as.POSIXct(index$dataset_embargo)

        # Add the path relative to the data directory
        index$path <- with(index, file.path(zenodo_concept_id,
                                            zenodo_record_id, filename))

        # Identify availability and most recent available records
        index$available <- with(index,
                                ifelse(dataset_access == "embargo" &
                                       dataset_embargo >= Sys.time(), FALSE,
                                       ifelse(dataset_access == "restricted",
                                              FALSE, TRUE)))

        # Get the index rows by concept, reduce to the unique set of records
        # (dropping the multiple files), drop unavailable records, sort by
        # publication date and return the first zenodo_record_id.
        concepts <- split(subset(index, select = c(available, zenodo_record_id,
                                                   publication_date)),
                          f = index$zenodo_concept_id)

        mr_avail <- sapply(concepts, function(recs) {
            recs <- unique(recs)
            recs <- subset(recs, available)
            recs <- recs[order(recs$publication_date, decreasing = TRUE), ]

            if (nrow(recs)) {
                return(recs$zenodo_record_id[1])
            } else {
                return(numeric(0))
            }
        })

        mra <- with(index,
                    ifelse(zenodo_record_id %in% unlist(mr_avail),
                           TRUE, FALSE))
        index$most_recent_available <- mra

        # save the index into the cache
        assign("index", index, safedata_env)
    }

    invisible(index)
}


load_gazetteer <- function() {

    #' Load and cache the SAFE gazetteer
    #'
    #' This function loads the SAFE gazetteer, stored as a geojson file in the
    #' root of the SAFE data directory, as an \code{\link[sf]{sf}} GIS object.
    #' The GIS data uses the WGS84 (EPSG:4326) geographic coordinate system.
    #'
    #' The gazetteer contains the following fields:
    #' \describe{
    #' \item{location}{The official gazetteer name for a sampling site.}
    #' \item{type}{A short description of location type - typically the project
    #'       that created the location}
    #' \item{plot_size}{Where applicable, the size of the plot at a location.
    #'       Note that point locations may define a sampling area with a plot
    #'       size.}
    #' \item{display_order}{DELETE}
    #' \item{parent}{DELETE}
    #' \item{region}{One of the four major large scale sampling areas: SAFE,
    #'       Maliau, Danum and VJR}
    #' \item{fractal_order}{Only defined for the SAFE core sampling points,
    #'       which follow a fractal layout.}
    #' \item{transect_order}{Again, for SAFE core sampling points, the
    #'       location of a point along the sampling design transect.}
    #' \item{centroid_x, centroid_y}{The centroid of the feature}
    #' \item{source}{The original source GIS file that the feature was
    #'       described in.}
    #' \item{bbox_xmin, bbox_ymin, bbox_xmax, bbox_ymax}{The bounding box of
    #'       the feature.}
    #' \item{geometry}{The GIS geometry for the data - a column of class
    #'       \code{\link[sf]{sfc}}.}
    #' }
    #'
    #' When this function is first called in a session, the loaded
    #' \code{\link[sf]{sf}} object is cached for re-use (see
    #' \code{\link{load_index}}).
    #'
    #' @return An \code{\link[sf]{sf}} object containing the SAFE gazetteer
    #'    locations.
    #' @seealso \code{\link{load_location_aliases}}, \code{\link{load_index}}
    #' @export


    if (exists("gazetteer", safedata_env)) {
        gazetteer <- get("gazetteer", safedata_env)
    } else {
        safedir <- get_data_dir()
        gazetteer <- sf::st_read(file.path(safedir, "gazetteer.geojson"),
                                 quiet = TRUE, stringsAsFactors = FALSE)
        assign("gazetteer", gazetteer, safedata_env)
    }

    return(gazetteer)
}


load_location_aliases <- function() {

    #' Load and cache the SAFE location aliases
    #'
    #' This function loads the SAFE locations alias, stored as a csv file in
    #' the root of the SAFE data directory, as data frame.
    #'
    #' When this function is first called in a session, the loaded data frame
    #' object is cached for re-use (see \code{\link{load_index}}).
    #'
    #' @return A data frame containing the SAFE location aliases.
    #' @seealso \code{\link{load_gazetteer}}, \code{\link{load_index}}
    #' @keywords internal

    if (exists("location_aliases", safedata_env)) {
        location_aliases <- get("location_aliases", safedata_env)
    } else {
        safedir <- get_data_dir()
        alias_path <- file.path(safedir, "location_aliases.csv")
        location_aliases <- utils::read.csv(alias_path,
                                            na.strings = "null",
                                            stringsAsFactors = FALSE,
                                            colClasses = "character")
        assign("location_aliases", location_aliases, safedata_env)
    }

    return(location_aliases)
}


get_data_dir <- function() {

    #' Checks the data directory is set and returns it
    #'
    #' @keywords internal

    ddir <- getOption("safedata.dir")

    if (is.null(ddir)) {
        stop("SAFE data directory not set.")
    }

    return(ddir)
}


verbose_message <- function(str, ...) {

    #' Message function that can be globally muted
    #'
    #' Prints a message if  \code{option("safedata.verbose")}  is set
    #' to TRUE. Note that individual expressions can be muted using
    #' \code{suppressMessages()} but this mutes them globally.
    #'
    #' @keywords internal

    if (getOption("safedata.verbose")) {
        message(str, ...)
    }
}


set_example_safe_dir <- function() {

    #' Functions to use an example data directory for package examples
    #'
    #' The documentation of \code{safedata} includes code examples using a data
    #' directory. A zipped example directory is included in the package files
    #' (data/safedata_example_dir.zip) but package code must not write within
    #' the package structure. The \code{set_example_safe_dir} function is used
    #' in code examples to unpack this example directory into a temporary
    #' folder and set it for use in  the example code. The function
    #' \code{unset_demo_dir()} is then used to restore any existing data
    #' directory set by the user. The example directory should only be created
    #' once per session.
    #'
    #' @seealso \code{\link{set_safe_dir}}
    #' @export

    # record the user data directory if one has been set
    udir <- try(get_data_dir(), silent = TRUE)

    if (! inherits(udir, "try-error")) {
        options(safedata.user.dir = udir)
    }

    # Get the session tempdir and look for an existing extracted demo directory
    tdir <- tempdir()
    demo_dir <- file.path(tdir, "safedata_example_dir")

    if (! dir.exists(demo_dir)) {
        example_zip <- system.file("safedata_example_dir",
                                   "safedata_example_dir.zip",
                                   package = "safedata")
        utils::unzip(example_zip, exdir = tdir)
    }

    set_safe_dir(demo_dir, update = FALSE)
}

unset_example_safe_dir <- function() {

    #' @describeIn set_example_safe_dir Restores a user data directory after
    #'    running an code example.
    #' @export

    # retrieve the user directory and if it isn't null restore it
    udir <- getOption("safedata.user.dir")
    if (! is.null(udir)) {
        set_safe_dir(udir, update = FALSE)
    }
    load_index(reload = TRUE)
}
