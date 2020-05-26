
get_taxa <- function(obj){
    
    #' Obtaining taxonomy for a SAFE dataset.
    #'
    #' This function generates a taxonomy table for a specified SAFE dataset 
    #' record. Each row represent a taxon used in the data worksheets in the
    #' dataset and the table fields show the taxonomic hierarchy, taken from
    #' GBIF, for those taxa. 
    #'
    #' All SAFE datasets containing taxa must include a Taxa worksheet, which
    #' must contain all of the taxa referred to in taxon fields in data
    #' worksheets. All entries in this worksheet are  validated against the  
    #' GBIF database before publication and are used to populate the taxonomic
    #' index for the SAFE datasets. The validated taxonomic hierarchy for the
    #' data is also available in the metadata for a record and this function
    #' converts the metadata into a taxonomy table. If the Taxa worksheet for 
    #' a SAFE project dataset is empty, because the data does not contain 
    #' observations on taxa, \code{get_taxa} will return NULL.
    #'
    #' The taxonomy table includes the eight 'backbone' taxonomic ranks used
    #' in the GBIF database: Kingdom, Phylum, Class, Order, Family, Genus, 
    #' Species and Subspecies. It also includes three further fields:
    #' \code{taxon_name}, \code{taxon_rank} and \code{gbif_status}. These 
    #' record the original taxa provided in the Taxa worksheet and show
    #' where the taxonomic name used in a dataset differs from the canonical
    #' name used in GBIF. The row names of the taxon table are the labels
    #' used in data worksheets and are used to match a taxonomy table to a 
    #' loaded data worksheet (see \code{\link{add_taxa}}).
    #' 
    #' For more details on the structure of the Taxa worksheet see:
    #'
    #' \url{https://safedata-validator.readthedocs.io/en/latest/data_format/taxa/}
    #'
    #' @param obj A single record id, or an existing safedata dataframe.
    #' @return A taxonomy table of classes 'safe_taxa' and 'data.frame'.
    #' @seealso \code{\link{add_taxa}}
    #' @examples
    #'    set_example_safe_dir()
    #'    taxa <- get_taxa(1400562)
    #'    unset_example_safe_dir()
    #' @export
    
    if(inherits(obj, 'safedata')){
        record_set <- attr(obj, 'metadata')$safe_record_set
    } else {
        record_set <- validate_record_ids(obj)
    }
        
    if(nrow(record_set) != 1){
        stop("Requires a single valid record or concept id")
    } else if(is.na(record_set$record)){
        stop("Concept ID provided, please indicate a specific version")
    } 

    # Get the record metadata containing the taxon index
    taxa <- load_record_metadata(record_set)$taxa
    taxon_table <- taxon_index_to_taxon_table(taxa)
    
    return(taxon_table)
}

get_taxon_coverage <- function(){
    
    #' Retrieve the current taxon index 
    #'
    #' This function downloads the current taxon index across published  datasets
    #' and returns a formatted taxon table showing the taxonomic hierarchy of each
    #' taxon. It requires an internet connection.
    #'
    #' Note that the use of 'user' defined taxa can lead to some unusual entries. There
    #' is nothing to stop a user defining 'Gallus gallus' as a 'user' taxon with Animalia
    #' as a parent taxon. 
    #'
    #' @return A data frame containing higher taxon level information for each taxon
    #'   in the database taxon index.
    #' @seealso \code{\link{get_taxa}}
    #' @examples
    #'     \donttest{
    #'     all_taxa <- get_taxon_coverage()
    #'     }
    #' @export
    
    url <- getOption('safedata.url')
    api <- paste0(url, '/api/taxa')
    taxa <- try(jsonlite::fromJSON(api), silent=TRUE)
              
    if(inherits(taxa, 'try-error')){
        stop('Failed to download taxon index')
    }
    
    # This API does not bring in worksheet names (they will clash across datasets)
    # so replace them with the canonical taxon name
    taxa$worksheet_name <- taxa$taxon_name
    
    taxa <- taxon_index_to_taxon_table(taxa)

}


taxon_index_to_taxon_table <- function(taxa){
    
    #' Add taxon hierarchy to taxon metadata
    #'
    #' Taxon data is stored as a set of taxa with taxon ids and parent taxon
    #' ids. For any given dataset, the set should include all parent taxa required.
    #' This internal function takes such list of taxon data and adds columns showing
    #' the taxonomic hierarchy for each row. It is used by \code{\link{get_taxa}} and
    #' \code{\link{get_taxon_coverage}}.
    #'
    #' @param taxa An existing object of class \code{safedata}
    #' @return A data frame adding higher taxon level information for each taxon
    #' @seealso \code{\link{get_taxa}}
    #' @keywords internal
    
    if(length(taxa) == 0){
        return(NULL)
    }
     
    # create a taxon table to fill in
    worksheet_taxa <- unique(taxa$worksheet_name)
    worksheet_taxa <- worksheet_taxa[! is.na(worksheet_taxa)]
    taxon_table <- matrix(NA, ncol=13, nrow=length(worksheet_taxa), 
                          dimnames=list(
                              worksheet_taxa,
                              c("kingdom", "phylum", "class", "order", 
                                "family", "genus", "species", "subspecies", 
                                "variety", "form",  "taxon_name", "taxon_level",
                                "gbif_status")))
        
    # Identify the root - this is the set of taxa that have NA as parent id, which
    # should all be kingdoms or incertae_sedis
    is_root <- is.na(taxa$gbif_parent_id)
    root <- taxa[is_root,]
    taxa <- taxa[! is_root,]
    
    # Divide the remaining data frame up into a list giving the descendants from each parent id
    taxa <- split(as.data.frame(taxa), f=taxa$gbif_parent_id)
    
    # Now, starting with the root, work through the list of descendents.
    # The codes uses a stack with each layer representing a taxonomic level. 
    # These sets are stored in a stack with more specific taxa getting pushed onto
    # the top at index 1. The current name at each level is used as the stack list
    # name, so that names(stack) gives the current hierarchy. 
    
    # If the top of the stack was a named row in the taxonomy worksheet, the taxon
    # table is updated. Then we look for descendants of the top of stack or work 
    # through the other taxa at this or lower levels, looking for remaining descendants.
    
    # start the stack off
    stack <- list(list(top=root[1,], up_next=root[-1,]))
    names(stack) <- stack[[1]]$top$taxon_name
    
    
    while(length(stack)){
        
        current <- stack[[1]]$top
        current_wn <- current$worksheet_name
                
        # If the top of the stack has a worksheet name, add that to the taxon table
        if(! is.na(current_wn)){
            
            # set the taxon name, rank and status to put in the table
            txn <- current$taxon_name
            rnk <- current$taxon_rank
            sts <- current$gbif_status
            
            # Look for other taxa in up_next with the same worksheet name - these
            # are taxa which have a different GBIF canonical name to the one the
            # dataset provider used. Figure out which is which and update, otherwise
            # just use the singleton.
            pair_idx <- match(current_wn, stack[[1]]$up_next$worksheet_name)
            
            if(! is.na(pair_idx)){
                # remove the row from up next
                paired_taxon <- stack[[1]]$up_next[pair_idx,]
                stack[[1]]$up_next <- stack[[1]]$up_next[- pair_idx,]
                
                # If the current taxon is the GBIF accepted record, replace the 
                # taxon details with those provided in the dataset, else rename
                # the top of the stack with the accepted details
                if(current$gbif_status == 'accepted'){
                    txn <- paired_taxon$taxon_name
                    rnk <- paired_taxon$taxon_rank
                    sts <- paired_taxon$gbif_status
                } else {
                    names(stack)[1] <- paired_taxon$taxon_name
                }
            }
            
            # insert the names but drop user names for intermediate taxonomic ranks
            hierarchy <- c(rev(names(stack)), rep(NA, 10 - length(stack)))
            if(sts == 'user') hierarchy[length(stack)] <- NA
            taxon_table[current_wn, ] <- c(hierarchy, txn, rnk, sts)
        }
        
        
        # Does this taxon have descendents? If so, they move on to the top of the stack
        # otherwise, work back down the stack to find the next level with something
        # left in the up_next slot.
        descendants <- taxa[[as.character(current$gbif_id)]]
        
        if(! is.null(descendants)){
            descendants <- list(list(top=descendants[1,], up_next=descendants[-1,]))
            names(descendants) <- descendants[[1]]$top$taxon_name
            stack <- c(descendants , stack)
            
        } else {
            while(length(stack)){
                up_next <- stack[[1]]$up_next
                if(nrow(up_next)){
                    # If there is anything left in up_next, bring one up and replace the stack top
                    up_next <- list(list(top=up_next[1,], up_next=up_next[-1,]))
                    names(up_next) <- up_next[[1]]$top$taxon_name
                    stack <- c(up_next , stack[-1])
                    break
                } else {
                    # Otherwise pop off the top of the stack
                    stack <- stack[-1]
                }
            }
        } 
    }

    taxon_table <- as.data.frame(taxon_table, stringsAsFactors=FALSE)
    class(taxon_table) <- c('safe_taxa', 'data.frame')
    
    return(taxon_table)
    
}



add_taxa <- function (obj, taxon_field=NULL, taxon_table=NULL, prefix=NULL, which=NULL) {
      
    #' Add a taxonomic hierarchy to a SAFE data worksheet.
    #'
    #' All datasets containing taxon observations provide taxonomic data (see
    #' \code{\link{get_taxa}}) that can be linked to rows in data worksheets 
    #' that contain 'taxa' fields. This function matches the taxonomic hierarchy 
    #' for a dataset for a given taxon field in a data worksheet and inserts fields
    #' to show that hierarchy.
    #' 
    #' An existing taxon table can be provided to the function, but the table
    #' will be automatically loaded if no table is provided. If a data worksheet 
    #' only contains a single taxon field, then that field will be used
    #' automatically, otherwise users have to specify which taxon field to use.
    #' A prefix can be added to taxon fields in order to discrimate between fields
    #' from multuple taxon fields. By default, the function adds all of the fields
    #' included in the output of \code{\link{get_taxa}}, but \code{which} 
    #' allows a subset of field names to be used.
    #' 
    #' @param obj An existing object of class \code{safedata}
    #' @param taxon_field The name of a taxon field in a \code{safedata} for which
    #'    to add taxonomic data.
    #' @param taxon_table An existing taxon table for a dataset, as generated by 
    #'    \code{\link{get_taxa}}.
    #' @param prefix A string to be appended to taxon field names, primarily to
    #'    discriminate between fields of multiple taxonomies are to be added.
    #' @param which A vector specifying a subset of taxonomy table field names to add.
    #' @return A modified \code{safedata} object with added taxonomic columns.
    #' @seealso \code{\link{get_taxa}}
    #' @examples
    #'    set_example_safe_dir()
    #'    beetle_morph <- load_safe_data(1400562, 'MorphFunctTraits')
    #'    beetle_morph <- add_taxa(beetle_morph)
    #'    unset_example_safe_dir()
    #' @export
    
    if(! inherits(obj, 'safedata')){
        stop("add_taxa requires an object of class 'safedata'")
    }
    
    # Get the taxon field and check it
    obj_attr <- attr(obj, 'metadata')
    taxa_fields <- obj_attr$taxa_fields[[1]]
    
    if(length(taxa_fields) == 0){
        stop('Data frame does not contain taxon fields')
    } else if(is.null(taxon_field) && length(taxa_fields) == 1){
        taxon_field <- taxa_fields[1]
    } else if(is.null(taxon_field) && length(taxa_fields) > 1){
        stop('Data frame contains multiple taxon fields, specify taxon_field')
    } else if(! taxon_field %in% taxa_fields){
        stop(sprintf('%s is not a taxon field in the data frame', taxon_field))
    }
    
    # Load the taxonomy
    if(is.null(taxon_table)){
        taxon_table <- get_taxa(obj_attr$safe_record_set)
    } else if(! inherits(taxon_table, 'safe_taxa')){
        stop("'taxon_table' not a 'safe_taxa' object created by 'get_taxa'")
    }
    
    # This should never happen - would need a taxon field with no taxa worksheet
    if(is.null(taxon_table)){
        verbose_message('Dataset contains no taxonomic information.')
        return(obj)
    }
    
    # Match taxon names to the taxon and generate matching table
    idx <- match(obj[, taxon_field], rownames(taxon_table))
    
    if(any(is.na(idx))){
        stop('Not all taxa names found in taxon index, contact package developers.')
    }
    
    taxon_table <- taxon_table[idx, ]
  
    # Select fields if which is provided
    if(! is.null(which)){
        which_idx <- match(which, names(taxon_table))
        
        if(any(is.na(which_idx))){
            stop("Unknown taxon table fields in 'which'")
        }
        
        taxon_table <- subset(taxon_table, select=which_idx)
    }
    
    # Add prefix to fields if requested
    if(! is.null(prefix)){
        names(taxon_table) <- paste0(prefix, names(taxon_table))
    }
    
    # combine and copy across attributes (could create a method for cbind, but seems excessive!)
    ret <- cbind(obj, taxon_table)
    attr(ret, 'metadata') <- attr(obj, 'metadata')
    class(ret) <- c('safedata', 'data.frame')
    
    return(ret)
}

get_phylogeny <- function(record){
    
    #' Get a phylogeny for a dataset
    #'
    #' This function loads the taxa reported in a dataset (see \code{\link{get_taxa}})
    #' and creates a phylogeny for those taxa, using the \code{\link[ape]{phylo}} class. 
    #' Equal branch lengths are used. This function differs from \code{\link[ape]{as.phylo.formula}} 
    #' in that it does not expect all taxonomic levels to be non NA: tips can
    #' be at different taxonomic depths. The object has internal node labels showing
    #' the taxonomic hierarchy of the phylogeny.
    #'
    #' Note that the phylogeny will contain singleton nodes if an internal taxon has
    #' a single descendant: the \code{\link[ape]{ape}} functions \code{\link[ape]{has.singles}}
    #' and \code{\link[ape]{collapse.singles}} can be used to detect and remove these if 
    #' required.
    #' @examples
    #'    beetle_phylo <- get_phylogeny(1400562)
    #'    ape::plot.phylo(beetle_phylo, show.node.label=TRUE, font=1, no.margin=TRUE)
    #' @param record A single dataset record id
    #' @return An \code{\link[ape]{phylo}} object.
    #' @export
        
    record_set <- validate_record_ids(record)
            
    if(nrow(record_set) != 1){
        stop("Requires a single valid record or concept id")
    } else if(is.na(record_set$record)){
        stop("Concept ID provided, please indicate a specific version")
    } 

    # Get the record metadata containing the taxon index and if possible the taxon worksheet,
    # which provides the matching of taxon names to datasets
    taxa <- load_record_metadata(record_set)$taxa

    if(length(taxa) == 0){
        return(NULL)
    }

    # Need to generate an edge matrix and node labels. The phlyo node numbering
    # uses 1-N for the tips and (N+1):(N+n_internal) for the internal nodes, with
    # the numbering of internal nodes showing a nested structure (so N + 1 is the 
    # root). The number of tips is not known a priori (a taxon with a worksheet 
    # name is not necessarily a tip), so assign a correctly nested node numbering
    # and then renumber afterwards
    
    external <- logical(nrow(taxa))
    edge_matrix <- matrix(0, ncol=2, nrow=nrow(taxa) - 1)
    node_labels <- character(nrow(taxa))

    # This function autoincrements the index, guaranteeing that edges will
    # always have a smaller number in the parent node and hence providing an
    # ordering that can be used for renumbering to the ape structue:
    #   1-N are the tips and (N+1):(N+n_internal) are the nodes
    node_index <- 0
    node_id <- function() {node_index <<- node_index + 1; return(node_index)}
    edge_index <- 0
    edge_id <- function() {edge_index <<- edge_index + 1; return(edge_index)}

    # make a slot to store node id in the taxon table, so it can be updated
    # within the stack
    taxa$node_id <- 0
    
    # See get_taxa for notes on stack code   
    is_root <- is.na(taxa$gbif_parent_id)
    root <- taxa[is_root,]
    taxa <- taxa[! is_root,]
    taxa <- split(as.data.frame(taxa), f=taxa$gbif_parent_id)
    stack <- list(list(top=root[1,], up_next=root[-1,]))
    
    # Allocate the first node id and node_label
    root_node_id <- node_id()
    stack[[1]]$top$node_id <- root_node_id
    node_labels[root_node_id] <- stack[[1]]$top$taxon_name

    while(length(stack)){
        
        current <- stack[[1]]$top
        descendants <- taxa[[as.character(current$gbif_id)]]
                
        if(! is.null(descendants)){
            
            # fill in the edge matrix, allocating node_ids and setting taxon labels         
            for(idx in seq(nrow(descendants))){
                this_node <- node_id()
                node_labels[this_node] <- descendants[idx,]$taxon_name
                descendants[idx,]$node_id <- this_node
                edge_matrix[edge_id(), ] <- c(current$node_id, this_node)
            }
            
            # and then move descendants on to stack
            descendants <- list(list(top=descendants[1,], up_next=descendants[-1,]))
            names(descendants) <- descendants[[1]]$top$taxon_name
            stack <- c(descendants , stack)
            
        } else {
            # this is a tip, so set external and then work back down
            external[current$node_id] <- TRUE
            
            while(length(stack)){
                up_next <- stack[[1]]$up_next
                if(nrow(up_next)){
                    # If there is anything left in up_next, bring one up and replace the stack top
                    up_next <- list(list(top=up_next[1,], up_next=up_next[-1,]))
                    names(up_next) <- up_next[[1]]$top$taxon_name
                    stack <- c(up_next , stack[-1])
                    break
                } else {
                    # Otherwise pop off the top of the stack
                    stack <- stack[-1]
                }
            }
        } 
    }
    
    # Now need to renumber the nodes edge matrix: 1-N are the tips and (N+1):
    n_tips <- sum(external)
    n_internal <- sum(! external)
    
    # mapping to ape node numbers
    node_mapping <- numeric(length(node_labels))
    node_mapping[external] <- 1:n_tips
    node_mapping[! external] <- (n_tips + 1):(n_tips + n_internal)  
    ape_edge_matrix <- cbind(node_mapping[edge_matrix[,1]], node_mapping[edge_matrix[,2]])
    
    ret <- list(edge=ape_edge_matrix,
                Nnode=n_internal, 
                edge.length = rep(1, nrow(ape_edge_matrix)),
                tip.label=node_labels[external],
                node.label=node_labels[!external])
    
    class(ret) <- 'phylo'
    
    return(ret)
    
}
