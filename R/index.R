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
                         url = "https://www.safeproject.net",
                         use_zenodo_sandbox = FALSE) {
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
    #' The default behaviour is to download actual data files from the main
    #' Zenodo site, but a sandbox site is also provided for testing. The
    #' sandbox site may be useful in setting up a new safedata archive, so
    #' \code{use_zenodo_sandbox = TRUE} can be used to create a safedata
    #' directory that will download data from the sandbox site.
    #'
    #' @param safedir A path to the directory to set as the SAFE data
    #'    directory (str).
    #' @param update Should the local dataset index be updated (logical)?
    #' @param create Should a new data directory be created at the provided
    #'    path (logical)?
    #' @param url A URL providing the SAFE Data API, defaulting to the SAFE
    #'    Project's own URL.
    #' @param use_zenodo_sandbox A boolean indicating whether the datafiles
    #'    are stored in the main Zenodo site or the sandbox Zenodo site.
    #' @return Invisibly, a boolean showing whether a SAFE data directory was
    #'    set successfully.
    #' @export

    # Clear any cached data in the safedata environment
    cached <- ls(envir = safedata_env)
    if (length(cached) > 0) {
        do.call(rm, list(list = cached, envir = safedata_env))
    }

    # Get the expected index and data file paths
    index_path <- file.path(safedir, "index.json")
    gazetteer_path <- file.path(safedir, "gazetteer.geojson")
    location_aliases_path <- file.path(safedir, "location_aliases.csv")
    url_path <- file.path(safedir, "url.json")

    # Handle create first
    if (create) {
        # We don't want a existing directory that might have stuff in it
        if (dir.exists(safedir)) {
            stop("Directory already exists")
        }

        # create the directory and set the safe data directory and url
        dir.create(safedir)
        options(
            safedata.dir = safedir,
            safedata.url = url,
            safedata.use_zenodo_sandbox = use_zenodo_sandbox
        )

        # Save the URL and other settings
        settings <- list(url = url, use_zenodo_sandbox = use_zenodo_sandbox)
        jsonlite::write_json(settings, url_path)

        # download the index file, then cache it
        got_index <- download_index()
        got_gazetteer <- download_gazetteer()
        got_loc_aliases <- download_location_aliases()

        # If any of these fail then remove the directory since we already
        # checked it didn't exist, and then tidy up options
        if (!(got_index && got_gazetteer && got_loc_aliases)) {
            unlink(safedir, recursive = TRUE)
            options(safedata.dir = NULL, safedata.url = NULL)
            message(
                "Could not download required files: ",
                "SAFE data directory not created"
            )
            return(invisible(FALSE))
        }

        verbose_message("Safe data directory created")
        load_index()
        return(invisible(TRUE))
    }

    # Now validate an existing directory
    all_good <- TRUE

    if (!dir.exists(safedir)) {
        message("Directory not found.")
        all_good <- FALSE
    }

    if (!file.exists(url_path)) {
        message("API URL not found.")
        all_good <- FALSE
    }

    if (!file.exists(index_path)) {
        message("Dataset index not found.")
        all_good <- FALSE
    }

    if (!file.exists(gazetteer_path)) {
        message("Gazetteer not found.")
        all_good <- FALSE
    }

    if (!file.exists(location_aliases_path)) {
        message("Location aliases not found.")
        all_good <- FALSE
    }

    if (!all_good) {
        return(invisible(FALSE))
    }

    # Set the data directory and URL in options.
    settings <- jsonlite::read_json(url_path)
    options(
        safedata.dir = safedir,
        safedata.url = settings$url,
        safedata.use_zenodo_sandbox = settings$use_zenodo_sandbox
    )

    # Look for updates
    if (update) {
        verbose_message("Checking for updates")

        # Try to get the current index hashes from the SAFE Data API and
        # then check each of the three index files
        index_hashes <- try_to_download(paste0(url, "/api/index_hashes"))

        if (isFALSE(index_hashes)) {
            message("Unable to download updates, using existing index files: ")
            message(attr(index_hashes, "fail_msg"))
            load_index()
            return(invisible(TRUE))
        } else {
            index_hashes <- httr::content(index_hashes)
        }

        # Update the indexing files as an atomic commit - there is unlikely
        # to be an issue in having non-contemporary versions, but why risk it
        update_successful <- TRUE
        backed_up <- character()

        # Check the files - do exactly the same thing for three files, three
        # hashes, three download wrapper functions and three reporting names
        update_details <- list(
            list(
                "Index",
                index_path,
                index_hashes$index,
                download_index
            ),
            list(
                "Gazetteer",
                gazetteer_path,
                index_hashes$gazetteer,
                download_gazetteer
            ),
            list(
                "Location aliases",
                location_aliases_path,
                index_hashes$location_aliases,
                download_location_aliases
            )
        )

        for (details in update_details) {
            fname <- details[[1]]
            local_path <- details[[2]]
            current_hash <- details[[3]]
            download_func <- details[[4]]

            if (tools::md5sum(local_path) != current_hash) {
                verbose_message(sprintf(" - Updating %s", fname))
                # Save the current version in case we need to roll back
                file.rename(local_path, paste0(local_path, ".oldbkp"))
                backed_up <- c(backed_up, local_path)
                # Try and get the new version
                got_update <- download_func()
                if (!got_update) {
                    update_successful <- FALSE
                }
            } else {
                verbose_message(sprintf(" - %s up to date", fname))
            }
        }

        if (!update_successful) {
            # Delete successful downloads and restore backups
            for (local_path in backed_up) {
                if (file.exists(local_path)) {
                    file.remove(local_path)
                }
                file.rename(paste0(local_path, ".oldbkp"), local_path)
            }
            message("Unable to download updates, using existing index files")
        } else {
            # remove backups
            for (local_path in backed_up) {
                file.remove(paste0(local_path, ".oldbkp"))
            }
            message("Index files successfully updated")
        }
    }

    # Load the index
    load_index()

    return(invisible(TRUE))
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
    api <- paste0(url, "/api/metadata_index.json")

    success <- try_to_download(api, path)

    if (!success) {
        message("Failed to download index:")
        message(attr(success, "fail_msg"))
    }

    return(success)
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
    api <- paste0(url, "/api/gazetteer.json")

    success <- try_to_download(api, path)

    if (!success) {
        message("Failed to download gazetteer:")
        message(attr(success, "fail_msg"))
    }

    return(success)
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
    api <- paste0(url, "/api/location_aliases.json")
    success <- try_to_download(api, path)

    if (!success) {
        message("Failed to download location aliases:")
        message(attr(success, "fail_msg"))
    }

    return(success)
}


load_index <- function() {
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
    #' @return Returns NULL invisibly - use get_index() to obtain the
    #'    index data.
    #' @seealso \code{\link{load_location_aliases}},
    #'    \code{\link{load_gazetteer}}
    #' @keywords internal

    verbose_message("Loading and caching index")
    # Load from file and convert into data frame
    safedir <- get_data_dir()

    # path expand to make paths work in md5sum, which fails with ~/.
    safedir <- path.expand(safedir)

    index_path <- file.path(safedir, "index.json")
    gazetteer_path <- file.path(safedir, "gazetteer.geojson")
    location_aliases_path <- file.path(safedir, "location_aliases.csv")
    url_path <- file.path(safedir, "url.json")

    index_path <- file.path(safedir, "index.json")

    index <- jsonlite::fromJSON(index_path)

    # format the datetime classes
    index$publication_date <- as.POSIXct(index$publication_date)
    index$dataset_embargo <- as.POSIXct(index$dataset_embargo)

    # Add the path relative to the data directory
    index$path <- with(index, file.path(
        zenodo_concept_id,
        zenodo_record_id, filename
    ))

    # Identify availability and most recent available records
    index$available <- with(
        index,
        ifelse(dataset_access == "embargo" &
            dataset_embargo >= Sys.time(), FALSE,
        ifelse(dataset_access == "restricted",
            FALSE, TRUE
        )
        )
    )

    # Get the index rows by concept, reduce to the unique set of records
    # (dropping the multiple files), drop unavailable records, sort by
    # publication date and return the first zenodo_record_id.
    concepts <- split(
        subset(index, select = c(
            available, zenodo_record_id,
            publication_date
        )),
        f = index$zenodo_concept_id
    )

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

    mra <- with(
        index,
        ifelse(zenodo_record_id %in% unlist(mr_avail),
            TRUE, FALSE
        )
    )
    index$most_recent_available <- mra

    # Validate the directory contents
    verbose_message("Validating directory")

    # Run a check on directory structure
    local_files <- dir(safedir, recursive = TRUE)

    # Exclude the three index files and local metadata json files
    index_files <- c(
        basename(index_path), basename(gazetteer_path),
        basename(location_aliases_path), basename(url_path)
    )
    json_files <- grepl("[0-9]+/[0-9]+/[0-9]+.json$", local_files)
    metadata_json <- local_files[json_files]
    local_files <- setdiff(local_files, c(index_files, metadata_json))

    # Check for additional files in directory structure
    local_unexpected <- setdiff(local_files, index$path)
    if (length(local_unexpected)) {
        warning(
            "SAFE data directory contains unexpected files: ",
            paste(local_unexpected, collapse = ", ")
        )
    }

    # Run a check on file modification
    local_expected <- subset(index, file.exists(file.path(safedir, index$path)))
    local_expected$md5 <- tools::md5sum(file.path(safedir, local_expected$path))
    local_expected$altered <- with(local_expected, md5 != checksum)

    if (sum(local_expected$altered)) {
        warning(
            "Local copies of dataset files have been modified",
            paste(local_expected$filename[local_expected$altered],
                collapse = ", "
            )
        )
    }

    # Update index to note which files have unaltered local copies (this
    # could include embargoed and restricted datasets, so this is used as a
    # flag to note which can be loaded from private local copies).
    local_unaltered <- local_expected$checksum[!local_expected$altered]
    index$local_copy <- index$checksum %in% local_unaltered

    # save the index into the cache
    assign("index", index, safedata_env)

    invisible()
}


get_index <- function() {
    #' Get the cached dataset index
    #'
    #' This function just safely retrieves the safedata index from the
    #' package cache.
    #'
    #' @return A data frame containing the dataset index details.
    #' @seealso \code{\link{load_index}}
    #' @keywords internal

    if (exists("index", safedata_env)) {
        # Load from cache in safedata_env environment
        return(get("index", safedata_env))
    } else {
        # Something is wrong
        stop("The safedata index is not loaded.")
    }
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
            quiet = TRUE, stringsAsFactors = FALSE
        )
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
            colClasses = "character"
        )
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
    #' \code{unset_example_safe_dir} is then used to restore any existing data
    #' directory set by the user. The example directory should only be created
    #' once per session.
    #'
    #' @seealso \code{\link{set_safe_dir}}
    #' @return The set_example_safe_dir function returns the path of the example
    #'    directory invisibly.
    #' @export

    # record the user data directory if one has been set
    udir <- try(get_data_dir(), silent = TRUE)

    if (!inherits(udir, "try-error")) {
        options(safedata.user.dir = udir)
    }

    # Get the session tempdir and look for an existing extracted demo directory
    tdir <- tempdir()
    demo_dir <- file.path(tdir, "safedata_example_dir")

    if (!dir.exists(demo_dir)) {
        example_zip <- system.file("safedata_example_dir",
            "safedata_example_dir.zip",
            package = "safedata"
        )
        utils::unzip(example_zip, exdir = tdir)
    }

    set_safe_dir(demo_dir, update = FALSE)
    return(invisible(demo_dir))
}

unset_example_safe_dir <- function() {
    #' @describeIn set_example_safe_dir Restores a user data directory after
    #'    running an code example.
    #' @export

    # retrieve the user directory and if it isn't null restore it
    udir <- getOption("safedata.user.dir")
    if (!is.null(udir)) {
        set_safe_dir(udir, update = FALSE)
    }
}
