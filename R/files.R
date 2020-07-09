get_file_details <- function(record_id) {

    #' Get file details for a record
    #'
    #' This function returns a list of the files associated with a record.
    #' The access status of the record is reported, along with the local
    #' absolute file path and whether a local copy is present. If there
    #' is no local copy and the dataset is open access, you can use
    #' \code{\link{download_safe_files}} to get local copies. If the dataset
    #' is embargoed or restricted, see \code{\link{insert_dataset}}.
    #'
    #' @param record_id A record id
    #' @return A data frame with fields: filename, dataset_access, path
    #'    and local.
    #' @examples
    #'    \donttest{
    #'    set_example_safe_dir()
    #'    files <- get_file_details(1400562)
    #'    unset_example_safe_dir()
    #'    }
    #' @export

    # validate the record id
    record_set <- validate_record_ids(record_id)
    index <- load_index()

    # Check for a single _record_ id
    if (nrow(record_set) != 1 | is.na(record_set$record)) {
        stop("Requires a single valid record id - not a concept id")
    }

    files <- subset(index, zenodo_record_id == record_set$record,
                    select = c(filename, dataset_access, path))
    files$path <- file.path(getOption("safedata.dir"), files$path)
    files$local <- file.exists(files$path)

    return(files)
}
