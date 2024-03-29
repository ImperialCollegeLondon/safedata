#' Finding and using data from the SAFE Project.
#'
#' @description
#' The SAFE Project is one of the largest ecological experiments in the world.
#' We are studying how biodiversity and ecosystem function change as forests are
#' modified by human activities. As well as studying the change, we also
#' explore whether preserving sections of forest within modified landscapes
#' and around waterways can protect biodiversity and ecosystem function,
#' and how much protection is needed to be effective.
#'
#' The project is a multinational collaboration that has generated a wide range
#' of research data. We archive and document this data using the Zenodo file
#' repository and maintain spatial, taxonomic and other indices to support data
#' discovery and reuse. This package provides a range of tools to make it easier
#' to find and analyse our research data outputs.
#'
#' @section Links:
#'    \url{https://www.safeproject.net/datasets/view_datasets}
#'    \url{https://www.zenodo.org/communities/safe}
#'
#' @docType package
#' @name safedata


# All of the variables below appear in non standard evaluation formats
# (e.g. subset etc.) and so get NOTEd by R CMD CHECK. This suppresses the
# NOTEs, but I won't pretend I'm happy about the solution.

if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(
        "available",
        "checksum",
        "col_idx",
        "concept",
        "current_safe_dir_path",
        "dataset_access",
        "dataset_embargo",
        "dataset_title",
        "description",
        "field_name",
        "field_type",
        "filename",
        "full_path",
        "gbif_id",
        "gbif_status",
        "geometry",
        "head",
        "local_copy",
        "local_path",
        "location",
        "max_col",
        "md5",
        "most_recent_available",
        "mra",
        "n_data_row",
        "name",
        "path",
        "publication_date",
        "record",
        "safe_record_set",
        "str",
        "taxon_name",
        "taxon_rank",
        "to_download",
        "worksheet_name",
        "zenodo_concept_id",
        "zenodo_record_id"
    ))
}
