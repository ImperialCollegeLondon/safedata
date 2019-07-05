

get_taxonomy <- function(record_id){
    
    #' Get a data frame containing the taxonomy for a dataset
    #'
    #' The record metadata from the SAFE Project website API includes
    #' the taxon index for the dataset. This function turns that set of entries
	#' into a data frame, containing the backbone taxonomy used within GBIF and
	#' the taxon name used within the dataset to make it easy to merge onto data 
	#' tables from the dataset. 
	#'
	#' Note that the taxon name used in the dataset is not necessarily the same
	#' as the canonical GBIF ID for the taxon.
    #'
    #' Note that the \code{ape} function \code{as.phylo.formula} can be used
    #' to turn this structure into a 'phylogeny'.
    #' @param record_id A SAFE dataset record id 
    #' @return A dataframe of the taxa used in the dataset, with columns
	#'   giving the GBIF backbone taxonomic levels (kingdom, phylum, order, class, 
	#'   family, genus, species) along with the original taxon names used in the
	#'   dataset.
    #' @export
    
	
	record_set <- validate_record_ids(record_id)
	
	if(nrow(record_set) != 1){
		stop('show_record requires a single record id')
	}
	
	if(all(is.na(record_set))){
		stop('Unknown record id')
	} else if(is.na(record_set$record)){
		stop('show_record requires record id not a concept id')		
	}
			
	# Get the record metadata and a single row for the record
	taxa <- load_record_metadata(record_set)$taxa
	
    if(length(taxa) == 0){
        return(NA)
    } else {
        taxa <- data.frame(taxa, stringsAsFactors=FALSE)
        names(taxa) <- c('id','pid','name','level','status','asname','aslevel')
        
        # Note which rows are leaves - their ID does not appear in the parent ID column
        taxa$leaf <- ! taxa$id %in% taxa$pid
        total_leaves <- sum(taxa$leaf)
        
        # Generate the asname column - this should merge on to the taxon names
        # provided in the dataset.
        taxa$asname <- ifelse(is.na(taxa$asname), taxa$name, taxa$asname)
        
        # Divide the data frame up into a list giving the descendants from each parent id
        taxa <- split(as.data.frame(taxa), f=taxa[,2])
        
        # Now, starting with the parent index of -1 for the root, work through that
        # list of descendants. Get a set of descdendants, add any leaves to the final
        # output and then split what is left into the current top and remaining taxa.
        # These sets are stored in a stack with more specific taxa getting pushed onto
        # the top at index 1. The current name at each level is used as the stack list
        # name, so that names(stack) gives the current hierarchy.
        
        # start the stack off
        push <- taxa[["-1"]]
        stack <- list(list(top=push[1,], up_next=push[-1,]))
        names(stack) <- stack[[1]]$top$name
        
        # create a leaf table to fill in
        leaf_table <- matrix(NA, ncol=10, nrow=total_leaves)
        leaf_idx <- 0
        
		
        while(length(stack)){
			cat(names(stack), '\n')
			
            if(stack[[1]]$top$leaf){
                # If the top of the stack is a leaf, write it out into the leaf table
                hierarchy <- c(rev(names(stack)), rep(NA, 8 - length(stack)))
                leaf_idx <- leaf_idx + 1
                leaf_table[leaf_idx, ] <- c(hierarchy, stack[[1]]$top$asname, stack[[1]]$top$aslevel)
                
                # Now work back down the stack to find the next level with something
                # left in the up_next slot.
                while(length(stack)){
                    push <- stack[[1]]$up_next
                    if(nrow(push)){
                        # ii) If there is anything left in up_next, bring one up and replace the stack top
                        push <- list(list(top=push[1,], up_next=push[-1,]))
                        names(push) <- push[[1]]$top$name
                        stack <- c(push , stack[-1])
                        break
                    } else {
                        # iii) Otherwise pop off the top of the stack
                        stack <- stack[-1]
                    }
                }
            } else {
                # Otherwise, if the top of the stack isn't a leaf,  stick its descendants onto the stack
                push <- taxa[[stack[[1]]$top$id]]
                push <- list(list(top=push[1,], up_next=push[-1,]))
                names(push) <- push[[1]]$top$name
                stack <- c(push , stack)
            }
        }

        leaf_table <- as.data.frame(leaf_table, stringsAsFactors=FALSE)
        names(leaf_table) <- c("kingdom", "phylum", "class", "order", "family", "genus", "species", "subspecies", "taxon_name", "taxon_level")
        return(leaf_table)
    }
}


addTaxa <- function (obj, filePath = NULL) {
  #' Add taxonomic observations to a \code{safedata} object
  #'
  #' This function adds taxonomic data from the Taxa worksheet in a SAFE project
  #' dataset - in the form of a table - to an existing \code{safedata} object.
  #' If no Taxa worksheet is provided with the dataset, the table defaults to
  #' \code{NA}.
  #' 
  #' @param obj An existing object of class \code{safedata}
  #' @param filePath The complete path to the SAFE dataset .xlsx file. By 
  #'   default this value is set to \code{NULL} and the function attempts to 
  #'   access the \code{obj$filePath} variable (i.e. the file pointer stored in
  #'   the \code{safedata} object itself). When a \code{filePath} is supplied, 
  #'   it must point to a SAFE .xlsx file, otherwise an error is returned.
  #' @return A modified \code{safedata} object with a \code{Taxa} dataframe
  #' @note Not all SAFE project submissions contain the Taxa worksheet (for
  #'   example if the data do not contain any taxonomic observations). In this
  #'   case \code{safedata$Taxa} defaults to \code{NA}.
  #' @seealso \code{\link{addTaxonHeirarchies}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/taxa/}
  #'   for information on the Taxa worksheet, \code{\link{addTaxonHeirarchies}}
  #' @export
  
  if(! inherits(obj, 'safedata')){
      stop("addTaxa requires an object of class 'safedata'")
  }
  
  # check file path
  if (is.null(filePath)) {
    if (!is.null(obj$filePath)) {
      filePath <- obj$filePath
    } else {
      stop(paste0('filePath not specified! Please check obj$filePath or pass a',
                  ' valid path to a SAFE dataset'))
    }
  } else {
    if (!grepl(".xlsx", filePath)) {
      stop(paste0('filePath must point to a SAFE .xlsx workbook! Please check ', 
                  'the path entered:', filePath))
    }
  }
  
  # get Taxa
  obj$Taxa <- tryCatch(
    {
      as.data.frame(readxl::read_xlsx(filePath, 'Taxa', col_names = TRUE))
    },
    error = function(e) {
      warning('SAFE import note - No Taxa datasheet exists!',
              call. = FALSE, immediate. = TRUE)
      return(NA)
    }
  )
  return(obj)
}

getNameBackbone <- function(taxaRow, ...) {
  #' Return taxonomic heirarchy for a SAFE taxonomic entry
  #' 
  #' This function attempts to perform a search of the GBIF database for the
  #' named Taxon/Parent types in the SAFE project Taxa worksheet. The purpose
  #' of this is to construct a taxonomic tree for all taxa reported in the
  #' dataset(s) to facilitate data searching/subsetting.
  #' 
  #' @param taxaRow A row from a SAFE object Taxa worksheet
  #' @param ... Optional arguments to pass to \code{\link[rgbif]{name_backbone}}
  #' @return GBIF name backbone list
  #' 
  #' @seealso \code{\link[rgbif]{name_backbone}},
  #'   \code{\link[rgbif]{name_usage}},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/data_format/taxa/},
  #'   \url{https://safe-dataset-checker.readthedocs.io/en/latest/safedataset_checker/gbif_validation/}
  #'   
  #' @note This function can take a few seconds to run, particularly if there
  #'   are a large number of Taxa in the SAFE dataset.
  #' 
  #' @export
  
  if (!is.na(taxaRow[['Parent name']])) {
    # The Parent Name has been provided. This means one of 3 things:
    # 1. The Taxon type is a morphospecies or functional group
    # 2. The Taxon name:type is unrecognized
    # 3. The Taxon is from a less common taxonomic level
    # In any case, taxonomic look-ups need to be done at the Parent level
    level = 'Parent'
  } else {
    # No Parent Name given so look-ups can be done on the provided Taxon
    level = 'Taxon'
  }
  
  # Taxonomic look-ups then follow a multi-step process
  # 1. strict search on the name backbone
  nameBackbone <- rgbif::name_backbone(name = taxaRow[[paste0(level, ' name')]],
                                       rank = taxaRow[[paste0(level, ' type')]], 
                                       strict = TRUE, verbose = FALSE, ...)
  
  # 2. if this search fails there are a few options
  if (nameBackbone$matchType == 'NONE') {
    # (i) attempt to match on Taxon/Parent ID (if this has been provided)
    # this is the case when there are multiple entries for the given name
    # first get all alternatives of the name using a strict search
    if (!is.na(taxaRow[[paste(level, 'ID')]])) {
      altOpts <- rgbif::name_backbone(name = taxaRow[[paste0(level, ' name')]],
                                      rank = taxaRow[[paste0(level, ' type')]], 
                                      strict = TRUE, verbose = TRUE, 
                                      ...)$alternatives
      # then match the ID against the GBIF entry within the alternatives
      nameBackbone <- altOpts[altOpts$usageKey==taxaRow[[paste0(level, ' ID')]],]
    } else {
      # (ii) when no Taxon/Parent ID is given, perform non-strict GBIF search
      altOpts <- rgbif::name_backbone(name = taxaRow[[paste0(level, ' name')]],
                                      rank = taxaRow[[paste0(level, ' type')]], 
                                      strict = FALSE, verbose = TRUE, ...)
      if (altOpts$data$matchType != 'NONE') {
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
  
  nameBackbone$safeName <- taxaRow[['Name']]
  return(nameBackbone)
}

nameBackboneToDf <- function (nameBackbone) {
  #' Convert RGBIF name_backbone list to dataframe
  #' 
  #' For improved readability and searchability, this function converts an
  #' \code{rgbif} name backbone (list) to a dataframe, storing information for
  #' the 8 main taxonomic levels (subspecies, species, genus, family, class,
  #' order, phylum, kingdom). The dataframe also includes the unique taxon
  #' identifier within the SAFE project submission and the \code{matchtype}
  #' variable returned from \code{rgbif}.
  #' 
  #' @param nameBackbone List of taxonomic heirarchy returned by 
  #'   \code{\link[rgbif]{name_backbone}}
  #' @return Taxonomic heirarchy stored as a dataframe including the 8 main
  #'   taxonomic levels
  #' @seealso \code{\link{addTaxonHeirarchies}}, \code{\link[rgbif]{name_backbone}}
  #' @export
  
  cols <- c('safeName', 'subspecies', 'species', 'genus', 'family', 'class', 
            'order', 'phylum', 'kingdom', 'matchType')
  taxaDf <- stats::setNames(data.frame(matrix(ncol = length(cols), nrow = 0), 
                                stringsAsFactors = FALSE), cols)
  rgbifDf <- as.data.frame(nameBackbone, stringsAsFactors = FALSE)
  return(dplyr::bind_rows(rgbifDf, taxaDf)[, cols])
}

getTaxonWrapper <- function (taxaRow, ...) {
  #' Extract taxonomic heirarchy from the GBIF database for the given taxa
  #' 
  #' This function is a wrapper that provides capabilities to extract taxonomic
  #' herirarchies from the GBIF database using Taxa worksheet in a SAFE data 
  #' file.
  #'
  #' @param taxaRow A row from a SAFE object Taxa worksheet
  #' @param ... Optional arguments to pass to \code{\link[rgbif]{name_backbone}}
  #' @return Dataframe containing the taxonomic heirarchy for the given taxon
  #' @seealso \code{\link{getNameBackbone}}, \code{\link{nameBackboneToDf}}
  #' @export
  
  listBackbone <- getNameBackbone(taxaRow, ...)
  return(nameBackboneToDf(listBackbone))
}

addTaxonHeirarchies <- function (obj, ...) {
  #' Add complete taxonomic heirarchies to a \code{safedata} object
  #' 
  #' Adds a dataframe of taxonomic heirarchies for all taxa reported in the SAFE
  #' dataset Taxa worksheet to the supplied \code{safedata} object. When no
  #' Taxa worksheet exists, this function flags a warning.
  #' 
  #' @param obj An existing object of class \code{safedata}
  #' @param ... Optional arguments to pass to \code{\link[rgbif]{name_backbone}}
  #' @return The updated \code{safedata} object with a dataframe of taxonomic
  #'   heirarchies for all taxa listed in the Taxa worksheet of the SAFE dataset.
  #'   This is accessed using the reference \code{TaxonHeirarchy}
  #' @note Not all SAFE datasets contain the Taxa worksheet. In this case, a
  #'   value of \code{NA} will be assigned to the \code{TaxonHeirarchy}
  #' @seealso \code{\link{addTaxa}}, \code{\link{getNameBackbone}}
  #' @export
  
  if(! inherits(obj, 'safedata')){
      stop("addTaxonHeirarchies requires an object of class 'safedata'")
  }
  
  if (is.null(obj$Taxa)){
    warning(paste0('Taxonomic data have not been added to the safedata object,',
                   ' please see addTaxa() function'))
  } else if (is.na(obj$Taxa)){
    warning('Cannot process Taxa: SAFE dataset contains no Taxa worksheet!')
    obj$TaxonHeirarchy <- NA
  } else {
    startT <- Sys.time()
    message('Starting taxonomy look-up... ', appendLF = FALSE)
    obj$TaxonHeirarchy <- 
      invisible(dplyr::bind_rows(apply(obj$Taxa, 1, getTaxonWrapper)))
    runT <- difftime(Sys.time(), startT, units = 'sec')
    message(sprintf('completed! This took %.2f seconds!', runT))
  }
  return(obj)
}
