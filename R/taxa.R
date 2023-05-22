
get_taxa <- function(obj) {
    #' Obtaining taxonomy for a SAFE dataset.
    #'
    #' This function generates a taxonomy table for a specified SAFE dataset
    #' record. Each row represent a taxon used in the data worksheets in the
    #' dataset and the table fields show the taxonomic hierarchy for those taxa.
    #'
    #' All SAFE datasets containing taxa must include worksheets providing
    #' taxonomy details, used to validate the taxa in the dataset against
    #' either or both of the GBIF or NCBI taxonomy database. All entries in
    #' these worksheet are  validated against the databases before
    #' publication and are used to populate a taxonomic index for safedata
    #' datasets. The validated taxonomic index for a specific dataset is
    #' also available in the metadata for a record and this function converts
    #' that metadata into a taxonomy table for the dataset.
    #'
    #' The returned table includes the fields \code{worksheet_name},
    #' \code{taxon_name}, \code{taxon_rank} and \code{taxon_status}, along
    #' with the taxonomy database used to validate the taxon
    #' (\code{taxon_auth}). These fields provide the original taxonomy
    #' worksheet data included.
    #'
    #' @section Synonyms:
    #' The taxon table will include extra rows for worksheet taxa that are
    #' GBIF synonyms or are merged in the NCBI taxonomy. There will be a row
    #' giving the taxonomic hierarchy for the original taxon name provided
    #' with the dataset and an additional row providing the canonical taxon
    #' name. The worksheet name for these rows will be identical.
    #'
    #' @section Taxonomic ranks:
    #' NCBI and GBIF differ in their use of taxonomic ranks: GBIF uses a
    #' reduced set of core 'backbone' ranks, where NCBI supports a wider
    #' range of ranks. The taxon hierarchy used for safedata reduces NCBI to
    #' the core GBIF backbone ranks but adds Superkingdom, which is used for
    #' bacterial and viral groups.
    #'
    #' Especially note that the taxonomy used by GBIF and NCBI are
    #' **not congruent**: if the same taxon is included using both systems,
    #' it may well have substantially different ranks.
    #'
    #' @details
    #' For more details on the structure of the taxon worksheets see:
    #' \url{https://safedata-validator.readthedocs.io/en/latest/data_format/taxa.html}
    #'
    #' @param obj A single record id, or an existing safedata dataframe.
    #' @return A taxonomy table of classes "safe_taxa" and "data.frame".
    #' @seealso \code{\link{add_taxa}}
    #' @examples
    #'     set_example_safedata_dir()
    #'     taxa <- get_taxa(1400562)
    #'     set_example_safedata_dir(on=FALSE)
    #' @export

    if (inherits(obj, "safedata")) {
        record_set <- attr(obj, "metadata")$safe_record_set
    } else {
        record_set <- validate_record_ids(obj)
    }

    if (nrow(record_set) != 1) {
        stop("Requires a single valid record or concept id")
    } else if (is.na(record_set$record)) {
        stop("Concept ID provided, please indicate a specific version")
    }

    # Get the record metadata containing the taxon index
    metadata <- load_record_metadata(record_set)

    # Turn the taxon indices to tables
    gbif_taxa <- taxon_index_to_taxon_table(metadata$gbif_taxa)
    ncbi_taxa <- taxon_index_to_taxon_table(metadata$ncbi_taxa)

    gbif_present <- ncbi_present <- FALSE

    # Add taxon auth and merge if needed
    if (!is.null(gbif_taxa)) {
        gbif_taxa$taxon_auth <- "GBIF"
        gbif_present <- TRUE
    }

    if (!is.null(ncbi_taxa)) {
        ncbi_taxa$taxon_auth <- "NCBI"
        ncbi_present <- TRUE
    }

    if (gbif_present && ncbi_present) {
        taxon_table <- rbind(gbif_taxa, ncbi_taxa)
    } else if (gbif_present) {
        taxon_table <- gbif_taxa
    } else if (ncbi_present) {
        taxon_table <- ncbi_taxa
    } else {
        return(NULL)
    }

    # Add record set metadata to table
    attr(taxon_table, "metadata") <- list(safe_record_set = record_set)

    return(taxon_table)
}

get_taxon_coverage <- function() {
    #' Retrieve the current taxon index
    #'
    #' This function downloads the current taxon index across published
    #' datasets and returns a formatted taxon table showing the taxonomic
    #' hierarchy of each taxon, along with the taxonomic authority used
    #' for validation (GBIF or NCBI). The returned data frame includes a
    #' count of the number of datasets that include each taxon. The
    #' function requires an internet connection.
    #'
    #' Note that the use of "user" defined taxa can lead to some unusual
    #' entries. There is nothing to stop a user defining "Gallus gallus" as a
    #' "user" taxon with Animalia as a parent taxon.
    #'
    #' @return A data frame containing taxonomic hierarchy data for
    #'   each taxon in the database taxon index. If the API is unavailable,
    #'   the function returns NULL.
    #' @seealso \code{\link{get_taxa}}
    #' @examples
    #'     \donttest{
    #'     all_taxa <- get_taxon_coverage()
    #'     }
    #' @export

    url <- getOption("safedata.url")
    api <- paste0(url, "/api/taxa")
    response <- try_to_download(api)

    if (isFALSE(response)) {
        message("Unable to download taxon index: ")
        message(attr(response, "fail_msg"))
        return(invisible())
    } else {
        # Use jsonlite::fromJSON (and simplification routines),
        # not the httr::content conversion
        taxon_index <- jsonlite::fromJSON(
            httr::content(response, as = "text", encoding = "UTF-8")
        )
    }

    # This API does not bring in worksheet names (they will clash across
    # datasets) so replace them with the canonical taxon name
    taxon_index$worksheet_name <- taxon_index$taxon_name
    taxon_table <- taxon_index_to_taxon_table(taxon_index)

    return(taxon_table)
}


taxon_index_to_taxon_table <- function(taxon_index) {
    #' Create a table showing taxon hierarchies from taxon metadata
    #'
    #' Taxon data is stored as a set of taxa with taxon ids and parent taxon
    #' ids. For any given dataset, the metadata includes an index containing
    #' this information for all taxa used in data worksheets and all parent
    #' taxa required. This internal function takes the taxon index metadata
    #' and builds a table including the taxonomic hierarchy for each worksheet
    #' taxon. It is used by \code{\link{get_taxa}} and
    #' \code{\link{get_taxon_coverage}}.
    #'
    #' Note that this function does not discriminate between NCBI and GBIF
    #' taxon indices: they are both mapped onto the same table structure.
    #' Public calling functions should handle any merging and tidying
    #' required for a neat public API.
    #'
    #' @param taxon_index A taxon index list from the dataset metadata,
    #' @return A data frame adding higher taxon level information for each taxon
    #' @seealso \code{\link{get_taxa}}
    #' @keywords internal

    if (length(taxon_index) == 0) {
        return(NULL)
    }

    # Get the backbone ranks
    table_fields <- c(
        "superkingdom", "kingdom", "phylum", "class", "order", "family",
        "genus", "species", "subspecies", "variety", "form"
    )
    n_fields <- length(table_fields)

    # Extract the worksheet taxa - all taxa that have a defined worksheet name
    worksheet_taxa <- taxon_index[!is.na(taxon_index$worksheet_name), ]

    # Build a dictionary of taxa, keyed by their taxon id, which has to be
    # as character here, because integers are assumed to be indices.
    taxa_lookup <- list()
    for (idx in seq_len(nrow(taxon_index))) {
        taxon <- as.list(taxon_index[idx, ])
        taxa_lookup[[as.character(taxon$taxon_id)]] <- taxon
    }

    # create a taxon table to fill in with columns keyed by taxon rank
    taxon_table <- matrix(NA,
        ncol = n_fields, nrow = nrow(worksheet_taxa),
        dimnames = list(NULL, table_fields)
    )

    # Loop over the rows of worksheet taxa, following the chain of parent
    # taxon ids down the lookup dictionary
    for (rw in seq_len(nrow(worksheet_taxa))) {
        # Record rank of row name if one of the core ranks
        row <- worksheet_taxa[rw, ]
        if (row$taxon_rank %in% table_fields) {
            taxon_table[rw, row$taxon_rank] <- row$taxon_name
        }

        # Loop over parents
        ptx <- row$parent_id

        while (!is.na(ptx)) {
            parent <- taxa_lookup[[as.character(ptx)]]
            taxon_table[rw, parent$taxon_rank] <- parent$taxon_name
            ptx <- parent$parent_id
        }
    }

    # Append information about the worksheet taxa to the table, handling extra
    # fields in the data for get_taxon_coverage
    tax_fields <- c(
        "worksheet_name", "taxon_name", "taxon_rank", "taxon_status"
    )
    if ("n_datasets" %in% names(worksheet_taxa)) {
        tax_fields <- c(tax_fields, "taxon_auth", "n_datasets")
    }

    taxon_table <- cbind(
        taxon_table,
        subset(worksheet_taxa, select = tax_fields)
    )
    class(taxon_table) <- c("safe_taxa", "data.frame")

    return(taxon_table)
}


add_taxa <- function(obj, taxon_field = NULL, taxon_table = NULL,
                     prefix = NULL, use_canon = FALSE) {
    #' Add a taxonomic hierarchy to a SAFE data worksheet.
    #'
    #' Data tables in a safedata dataset can contain "taxa" fields, which tie
    #' an observation to a particular taxon used in the dataset. All datasets
    #' containing taxon observations provide taxonomic data about those taxa,
    #' validated against either NCBI or GBIF. The taxon table for an entire
    #' dataset can be accessed using \code{\link{get_taxa}}, but this function
    #' allows the taxonomic hierachy to be added for the specific taxa used in
    #' in a taxa field in a data worksheet.
    #'
    #' An existing taxon table from \code{\link{get_taxa}} can be provided to
    #' this function, but the table will be automatically loaded if no table
    #' is provided. If a data worksheet only contains a single taxon field,
    #' then that field will be used automatically, otherwise users have to
    #' specify which taxon field to use. A prefix can be added to taxon fields
    #' in order to discrimate between taxon hierarchy fields added from
    #' different taxon fields.
    #'
    #' By default, the taxonomy data added to the table will use the original
    #' taxon name used in the dataset, even if that usage is considered a
    #' synonym or is merged in GBIF or NCBI (see \code{\link{get_taxa}}).
    #' The \code{use_canon} option can be set to TRUE to instead show the
    #' canonical taxon name used in the taxonomy database for synonomous or
    #' merged taxa.
    #'
    #' @param obj An existing object of class \code{safedata}
    #' @param taxon_field The name of a taxon field in a \code{safedata} for
    #'    which to add taxonomic data.
    #' @param taxon_table An existing taxon table for a dataset, as generated by
    #'    \code{\link{get_taxa}}.
    #' @param prefix A string to be appended to taxon field names, primarily to
    #'    discriminate between fields of multiple taxonomies are to be added.
    #' @param use_canon Add the canonical taxon name from the taxonomy database
    #'    rather than the name used in the dataset.
    #' @return A modified \code{safedata} object with added taxonomic columns.
    #' @seealso \code{\link{get_taxa}}
    #' @examples
    #'    set_example_safedata_dir()
    #'    beetle_morph <- load_safe_data(1400562, "MorphFunctTraits")
    #'    beetle_morph <- add_taxa(beetle_morph)
    #'    set_example_safedata_dir(on=FALSE)
    #' @export

    if (!inherits(obj, "safedata")) {
        stop("add_taxa requires an object of class 'safedata'")
    }

    # Get the taxon field and check it
    obj_attr <- attr(obj, "metadata")
    taxa_fields <- obj_attr$taxa_fields[[1]]

    if (length(taxa_fields) == 0) {
        stop("Data frame does not contain taxon fields")
    } else if (is.null(taxon_field) && length(taxa_fields) == 1) {
        taxon_field <- taxa_fields[1]
    } else if (is.null(taxon_field) && length(taxa_fields) > 1) {
        stop("Data frame contains multiple taxon fields, specify taxon_field")
    } else if (!taxon_field %in% taxa_fields) {
        stop(sprintf("%s is not a taxon field in the data frame", taxon_field))
    }

    # Load the taxonomy, making sure that an existing taxon table is the right
    # one for the worksheet
    if (is.null(taxon_table)) {
        taxon_table <- get_taxa(obj_attr$safe_record_set)
    } else if (inherits(taxon_table, "safe_taxa")) {
        taxa_record_set <- attr(taxon_table, "metadata")$safe_record_set
        if (!all.equal(taxa_record_set, obj_attr$safe_record_set)) {
            stop("Provided 'taxon_table' is for a different dataset.")
        }
    } else {
        stop("'taxon_table' not a 'safe_taxa' object created by 'get_taxa'")
    }

    # This should never happen - would need a taxon field with no taxa worksheet
    if (is.null(taxon_table)) {
        verbose_message("Dataset contains no taxonomic information.")
        return(obj)
    }

    # Drop canon/non_canon before matching
    non_canon_usage <- taxon_table$worksheet_name[
        duplicated(taxon_table$worksheet_name)
    ]
    canon_only <- !taxon_table$worksheet_name %in% non_canon_usage
    accepted_usage <- taxon_table$taxon_status == "accepted"
    if (use_canon) {
        taxon_table <- taxon_table[canon_only | accepted_usage, ]
    } else {
        taxon_table <- taxon_table[canon_only | !accepted_usage, ]
    }

    # Match taxon names in the taxon field to the worksheet names in the taxon
    # table and generate matching table
    idx <- match(obj[, taxon_field], taxon_table$worksheet_name)

    if (any(is.na(idx))) {
        stop(
            "Not all taxa names found in taxon index,",
            " contact package developers."
        )
    }

    # Get the ordered taxon table and drop the worksheet name field, which
    # will duplicate the taxon field
    taxon_table <- taxon_table[idx, ]
    taxon_table$worksheet_name <- NULL

    # Add prefix to fields if requested
    if (!is.null(prefix)) {
        names(taxon_table) <- paste0(prefix, names(taxon_table))
    }

    # combine and copy across attributes (could create a method for cbind,
    # but seems excessive!), dropping the worksheet name used for matching
    ret <- cbind(obj, taxon_table)
    rownames(ret) <- rownames(obj)
    attr(ret, "metadata") <- attr(obj, "metadata")
    class(ret) <- c("safedata", "data.frame")

    return(ret)
}


get_taxon_graph <- function(record, which = c("gbif", "ncbi")) {
    #' Get a graph of the taxa in a dataset
    #'
    #' This function loads the  taxon index for either of the NCBI or GBIF
    #' taxa present in a dataset (see \code{\link{get_taxa}}) and creates a
    #' taxonomic graph for the dataset using the \pkg{igraph} package.
    #'
    #' The graph vertices are the different taxa used in the dataset along
    #' with nodes for all parent taxa of those taxa. The graph vertex
    #' names are arbitrary strings (tx1, ...) but the vertex attributes
    #' contain the worksheet name, validated taxon name, taxon rank and taxon
    #' status (see \code{names(vertex_attr(obj))}).
    #'
    #' @examples
    #'    set_example_safedata_dir()
    #'    beetle_graph <- get_taxon_graph(1400562)
    #'    plot(beetle_graph, vertex.label.cex = 0.6, vertex.size = 15,
    #'         vertex.size2 = 3, vertex.shape = "rectangle")
    #'    # show worksheet names for tips
    #'    wsn <- igraph::vertex_attr(beetle_graph, "worksheet_name")
    #'    txn <- igraph::vertex_attr(beetle_graph, "taxon_name")
    #'    labels <- ifelse(is.na(wsn), txn, wsn)
    #'    is_leaf <- igraph::vertex_attr(beetle_graph, "leaf")
    #'    vert_col <- ifelse(is_leaf, "cornflowerblue", "grey")
    #'    plot(beetle_graph, vertex.label.cex = 0.6, vertex.size = 15,
    #'         vertex.size2 = 3, vertex.shape = "rectangle",
    #'         vertex.label = labels, vertex.color= vert_col)
    #'    set_example_safedata_dir(on=FALSE)
    #' @param which Return the GBIF or NCBI taxon graph
    #' @param record A single dataset record id
    #' @return An \code{\link[igraph:make_graph]{graph}} object.
    #' @export

    record_set <- validate_record_ids(record)

    if (nrow(record_set) != 1) {
        stop("Requires a single valid record or concept id")
    } else if (is.na(record_set$record)) {
        stop("Concept ID provided, please indicate a specific version")
    }

    # Get the record metadata containing the taxon index and if possible the
    # taxon worksheet, which provides the matching of taxon names to datasets
    which <- match.arg(which)
    md <- load_record_metadata(record_set)

    if (which == "gbif") {
        taxon_index <- md$gbif_taxa
    } else {
        taxon_index <- md$ncbi_taxa
    }

    if (length(taxon_index) == 0) {
        return(NULL)
    }

    # Remove duplicates:
    # 1) Exact duplicated entries in the taxon index are accidental user
    #    repeated taxa which is a legacy issue from older validation.
    taxon_index <- taxon_index[!duplicated(taxon_index), ]

    # 2) Rows can be duplicated _except_ for worksheet name. This occurs where
    #    a taxon is used as a worksheet taxa, but also occurs in the taxon
    #    hierarchy below other worksheet taxa. These represent the _same_
    #    vertex, and only the one with the worksheet name should be preserved.

    # Force NA worksheet names to the bottom of the index and then remove
    # rows that are duplicates of rows _with_ worksheet names but do not
    # have a worksheet name themselves.
    taxon_index <- taxon_index[order(taxon_index$worksheet_name), ]
    tidx_no_ws <- taxon_index[!names(taxon_index) == "worksheet_name"]
    taxon_index <- taxon_index[
        !duplicated(tidx_no_ws) |
            !is.na(taxon_index$worksheet_name),
    ]

    # Create a unique set of vertex names as a simple sequence. Worksheet
    # names are duplicated for canon/non-canon use and taxon_name can also
    # be duplicated from different usages.
    taxon_index$vertex_name <- paste0("tx", seq_len(nrow(taxon_index)))

    # Create an empty graph - directed by default, edges link from root to tip
    g <- igraph::make_empty_graph()

    # Populate with vertex data and add a root taxon
    g <- g + igraph::vertices(
        taxon_index$vertex_name,
        worksheet_name = taxon_index$worksheet_name,
        taxon_id = taxon_index$taxon_id,
        taxon_name = taxon_index$taxon_name,
        taxon_rank = taxon_index$taxon_rank,
        taxon_status = taxon_index$taxon_status
    )
    g <- g + igraph::vertex(
        "root",
        taxon_name = "root", worksheet_name = "root", taxon_rank = "root"
    )

    # Make a lookup table for vertex names from taxon id to use to add edges
    # from taxa to parent. Note that this will collapse user taxa with taxon_id
    # of -1, but those are always tips. Need to use character here to support
    # using taxon id as list names and to include root as parent
    taxon_index$taxon_id <- as.character(taxon_index$taxon_id)
    taxon_index$parent_id <- ifelse(
        is.na(taxon_index$parent_id), "root", taxon_index$parent_id
    )

    parent_lookup <- list("root" = "root")
    for (tx in seq_len(nrow(taxon_index))) {
        parent_lookup[taxon_index$taxon_id[tx]] <- taxon_index$vertex_name[tx]
    }

    # Add edges
    edges <- character()
    for (tx in seq_len(nrow(taxon_index))) {
        edges <- c(
            edges,
            parent_lookup[[taxon_index$parent_id[tx]]],
            taxon_index$vertex_name[tx]
        )
    }

    g <- igraph::add_edges(g, edges)

    # Add leaf node attributes
    igraph::vertex_attr(g, "leaf") <- igraph::degree(g, mode = "out") == 0

    if (!igraph::is_dag(g)) {
        warning("Taxon graph is not a directed acyclic graph")
    }

    if (!igraph::is_connected(g)) {
        warning("Taxon graph is not connected")
    }

    if (!igraph::is_simple(g)) {
        warning("Taxon graph is not simple")
    }

    return(g)
}


igraph_to_phylo <- function(g, labels = "taxon_name") {
    #' Getting a phylogeny for a dataset
    #'
    #' The function \code{igraph_to_phylo} takes a taxon graph (see
    #' \code{\link{get_taxon_graph}}) and attempts to convert that to a
    #' a \code{\link[ape:read.tree]{phylo}} object from \pkg{ape}.
    #' This will fail if the graph is not simple (no loops or multiple edges)
    #' or is not connected (has isolated taxa). Neither of these conditions
    #' should happen in datasets but they do.
    #'
    #' The phylogeny is assigned with equal branch lengths and so displays
    #' showing the taxonomic hierarchy of taxa. Note that the phylogeny will
    #' contain singleton nodes if an internal taxon has a single descendant -
    #' see the example below showing internal node labels. The
    #' \code{\link[ape:ape-package]{ape}} functions
    #' \code{\link[ape:collapse.singles]{has.singles}} and
    #' \code{\link[ape]{collapse.singles}} can be used to detect and remove
    #' these if required.
    #'
    #' The function \code{get_phylogeny} is simply a wrapper that calls
    #' \code{\link{get_taxon_graph}} and then \code{igraph_to_phylo}.
    # \
    #' @param labels The name of a vertex attribute from
    #'    \code{\link{get_taxon_graph}} to use as node labels.
    #' @seealso \code{\link{get_taxon_graph}}
    #' @examples
    #'    set_example_safedata_dir()
    #'    beetle_graph <- get_taxon_graph(1400562)
    #'    beetle_phylo <- igraph_to_phylo(beetle_graph)
    #'    ape::plot.phylo(beetle_phylo, show.node.label = TRUE)
    #'    # Or wrapped into a single function
    #'    beetle_phylo <- get_phylogeny(1400562)
    #'    ape::plot.phylo(beetle_phylo, show.node.label = TRUE)
    #'    set_example_safedata_dir(on=FALSE)
    #' @param g A taxon graph returned by \code{\link{get_taxon_graph}}
    #' @param record A single dataset record id
    #' @return An \code{\link[ape:read.tree]{phylo}} object.
    #' @export

    if (!igraph::is_simple(g) |
        !igraph::is_connected(g) |
        !igraph::is_dag(g)) {
        stop("Taxon graph is not a simple, connected, directed acylic graph")
    }

    # Use a depth first search of the tree to get an ordering
    # from the root to the tips
    traverse <- igraph::dfs(g, "root")

    # Use the leaf attribute to find which vertices in the sequence
    # are tips and then use this to create increasing numberings for the
    # tips and nodes. The sequences can then be merged to get a correct
    # phylo node numbering.
    is_leaf <- igraph::vertex_attr(g, "leaf", traverse$order)
    n_leaf <- sum(is_leaf)
    n_node <- sum(!is_leaf)
    node_id <- ifelse(is_leaf, cumsum(is_leaf), cumsum(!is_leaf) + n_leaf)

    # Store the node ids on the graph
    g <- igraph::set_vertex_attr(g, "node_id",
        index = traverse$order,
        value = node_id
    )

    # Extract the edge and vertex data
    vertex_data <- igraph::as_data_frame(g, "vertices")
    edge_data <- igraph::as_data_frame(g, "edges")

    # Substitute the node id numbers into the edge list
    edge_data <- unlist(edge_data)
    edge_data <- vertex_data$node_id[match(edge_data, vertex_data$name)]
    edge_data <- matrix(edge_data, ncol = 2)

    # lookup the tip and node labels
    labels <- vertex_data[, labels]
    tip_labels <- 1:n_leaf
    tip_labels <- labels[match(tip_labels, vertex_data$node_id)]
    node_labels <- (n_leaf + 1):(n_node + n_leaf)
    node_labels <- labels[match(node_labels, vertex_data$node_id)]

    # Build the phylogeny
    phy <- structure(
        list(
            edge = edge_data,
            edge.length = rep(1, nrow(edge_data)),
            tip.labels = tip_labels,
            node.labels = node_labels,
            Nnode = n_node
        ),
        class = "phylo"
    )
    return(phy)
}


get_phylogeny <- function(record, labels = "taxon_name") {
    #' @describeIn igraph_to_phylo Get a phylogeny for a dataset
    #' @export

    graph <- get_taxon_graph(record)
    phylo <- igraph_to_phylo(graph, labels = labels)

    return(phylo)
}
