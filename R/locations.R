load_gazetteer <- function(){
	
	#' Loads the SAFE gazetteer
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
	#' \item{region}{One of the four major large scale sampling areas: SAFE, Maliau, 
	#'       Danum and VJR}
	#' \item{fractal_order}{Only defined for the SAFE core sampling points, which 
	#'       follow a fractal layout.}
	#' \item{transect_order}{Again, for SAFE core sampling points, the location of 
	#'       a point along the sampling design transect.}
	#' \item{centroid_x, centroid_y}{The centroid of the feature}
	#' \item{source}{The original source GIS file that the feature was described in.}
	#' \item{bbox_xmin, bbox_ymin, bbox_xmax, bbox_ymax}{The bounding box of the feature.}
	#' \item{geometry}{The GIS geometry for the data - a column of class \code{\link[sf]{sfc}}.}
	#' }
	#'
	#' When this function is first called in a session, the loaded \code{\link[sf]{sf}}
	#' object is cached for re-use (see \code{\link{safedata.env}}).
	#' 
	#' @return An \code{\link[sf]{sf}} object containing the SAFE gazetteer locations.
    #' @seealso \code{\link{load_location_aliases}}
	#' @example
	#'    safedir <- system.file('example_data_dir', package='safedata')
	#'    set_safe_dir(safedir)
	#'    gazetteer <- load_gazetteer()
	#' @export
	
	gazetteer <- try(get('gazetteer', safedata.env), silent=TRUE)
	
	if(inherits(gazetteer, 'try-error')){
		
		safedir <- get_data_dir()
		gazetteer <- sf::st_read(file.path(safedir, 'gazetteer.geojson'), 
								 quiet=TRUE, stringsAsFactors=FALSE)
		
		assign('gazetteer', gazetteer, safedata.env)
	}
	
	return(gazetteer)
}


load_location_aliases <- function(){
	
	#' Loads the SAFE location aliases
	#'
	#' This function loads the SAFE locations alias, stored as a csv file in
	#' the root of the SAFE data directory, as data frame.
	#'
	#' When this function is first called in a session, the loaded data frame 
	#' object is cached for re-use (see \code{\link{safedata.env}}).
	#'
	#' @return A data frame containing the SAFE location aliases.
    #' @seealso \code{\link{load_gazetteer}}
	#' @example
	#'    safedir <- system.file('example_data_dir', package='safedata')
	#'    set_safe_dir(safedir)
	#'    aliases <- load_location_aliases()
	#' @export
	
	location_aliases <- try(get('location_aliases', safedata.env), silent=TRUE)
	
	if(inherits(location_aliases, 'try-error')){
		
		safedir <- get_data_dir()
		location_aliases <- read.csv(file.path(safedir, 'location_aliases.csv'), 
									 na.strings='null', stringsAsFactors=FALSE,
									 colClasses='character')
		assign('location_aliases', location_aliases, safedata.env)
	}
	
	return(location_aliases)
}


get_locations <- function(obj, gazetteer_info=FALSE){
    
    #' Obtaining locations for a SAFE dataset.
    #'
    #' This function returns a spatial 'simple features' (\code{\link[sf]{sf}})
	#' object for a dataset record, including basic information about locations
	#' for the SAFE gazetteer if requested.
	#'
	#' All SAFE datasets recording observations from the field must include a
	#' Locations worksheet, which must contain all of the location names used
	#' in Locations type fields in data worksheets. These entries are matched against
	#' existing location names in the SAFE gazetteer. Users can also include 
	#' 'new' location names in their dataset, which do not necessarily have
	#'  coordinates. Processing locations works as follows:
	#' \enumerate{
	#'     \item Known locations (\code{new_location=FALSE}) are matched against
	#'           both the official location names in the gazetteer and the 
	#'           accepted aliases.
	#'     \item New locations are first checked against the location aliases
	#'           list to see if any new locations in this dataset have been 
	#'           matched to a gazetteer location. The gazetteer location is
	#'           then used in preference to any new location data within the
	#'           dataset.
	#'     \item Finally, remaining new locations are assigned any feature 
	#' 			 geometry within the dataset. New locations do not necessarily
	#'           have geometry data, so may have null or empty geometries.
	#' }
	#' @param obj A single record id, or an existing safedata dataframe.
	#' @param gazetteer_info Should all the gazetteer fields be included in the 
	#'   returned \code{\link[sf]{sf}} object.
    #' @return An object of classes 'safe_locations', 'sf' and 'data.frame'.
    #' @note Not all SAFE project datasets contain locations. In this case
	#'   \code{\link{get_locations}} will return NULL.
    #' @seealso \code{\link{add_locations}}, \code{\link{load_gazetteer}}, 
	#'    \code{\link{load_location_aliases}}
	#' @example
	#'    safedir <- system.file('example_data_dir', package='safedata')
	#'    set_safe_dir(safedir)
	#'    locations <- get_locations(1400562)
    #' @export
    
    if(inherits(obj, 'safedata')){
        record_set <- attr(obj, 'safedata')$safe_record_set
    } else {
        record_set <- validate_record_ids(obj)
    }
        
    if(nrow(record_set) != 1){
        stop("Requires a single valid record or concept id")
    } else if(is.na(record_set$record)){
        stop("Concept ID provided, please indicate a specific version")
    } 

    # Get the record metadata containing the locations index 
    locations <- load_record_metadata(record_set)$locations
	locations$zenodo_record_id <- record_set$record

    if(length(locations) == 0){
        return(NULL)
    }
	
	# Load the gazeteer and location aliases
	gazetteer <- load_gazetteer()
	location_aliases <- load_location_aliases()
	
	# # Convert locations into an sf object, with an initially empty set of geometries
	# locations$geometry <- sf::st_sfc(NULL)
	# locations <- sf::st_sf(locations, crs='+init=EPSG:4326')
	
	# Now fill in the geometries from the possible sources
	# a) Known locations - look for gazetteer names and aliases
	known <- locations$new_location == FALSE
	gazetteer_match <- match(locations$name, gazetteer$location)
	alias_match <- match(locations$name, location_aliases$alias)
	
	if(any(known & is.na(gazetteer_match) & is.na(alias_match))){
		stop('Known locations with no gazetteer or aliases match found, contact data@safeproject.net')
	} else if(any(known & ! is.na(gazetteer_match) &  ! is.na(alias_match))){
		stop('Known locations match gazetteer _and_ aliases, contact data@safeproject.net')
	}
	
	# create a merge column for the gazetteer data
	locations$merge_gazetteer <- ifelse(known & ! is.na(gazetteer_match), 
										gazetteer$location[gazetteer_match], NA)
	locations$merge_gazetteer <- ifelse(known & ! is.na(alias_match), 
										location_aliases$location[alias_match], 
										locations$merge_gazetteer)

	# b) New locations that have now been aliased to a gazetteer location. These
	#    are locations that have been adopted into the gazetteer and are matched
	#    to the gazetteer by the unique combination of dataset record id and name
	#    within the dataset.
	
	new_locations <- locations[! known, ]
	
	new_locations <- merge(new_locations, location_aliases, 
						   by.x=c('zenodo_record_id', 'name'),
						   by.y=c('zenodo_record_id', 'alias'), all.x=TRUE)
	
	# switch in any aliased new locations
	new_locations <- subset(new_locations, ! is.na(new_locations$location))
	
	if(nrow(new_locations)){
		new_matched <- match(new_locations$name, locations$name)
		locations$merge_gazetteer[new_matched] <- new_locations$location
	}
	
	# Now we can merge the gazetteer data onto locations. First, optionally
	# exclude the bulky gazetteer data.
	
	if(! gazetteer_info){
		gazetteer <- subset(gazetteer, select=c(location, geometry))
	}
	
	# Next merge on the gazetteer, using all.y to ensure that all locations get the
	# gazetteer fields, with NA and  GEOMETRYCOLLECTION EMPTY if there is no match.
	# The reason for using all.y is that there is a bug in merge(data.frame, sf, 
	# all.x=TRUE) that causes it to fail with non matching lines, but merge(sf, 
	# data.frame, all.y=TRUE) works as expected.
	locations <- sf::st_sf(merge(gazetteer, locations, by.y='merge_gazetteer', 
					    		 by.x='location', all.y=TRUE))
	
 	# c) Finally, we can convert any WKT information from the locations table for
	#    new locations and put that into blanks in the geometry, specifically 
 	#    avoiding locations where step b) has inserted gazetteer data.
	wkt_data <- which((! is.na(locations$wkt_wgs84)) & (sf::st_is_empty(locations))) 
	
	if(length(wkt_data)){
		locations$geometry[wkt_data] <- sf::st_as_sfc(locations$wkt_wgs84[wkt_data])
	}
	
	# tidy and rename
	locations$zenodo_record_id <- NULL
	locations$wkt_wgs84 <- NULL
	nm <- names(locations)
	nm <- names(locations)
	nm[nm == 'location'] <- 'gazetteer_name'
	nm[nm == 'name'] <- 'dataset_name'
	names(locations) <- nm
	
	# add rownames, flag as a valid safe_locations table and return
	rownames(locations) <- locations$dataset_name
	class(locations) <- c('safe_locations', class(locations))
	return(locations)
	
}

add_locations <- function (obj, location_field=NULL, location_table=NULL, gazetteer_info=FALSE) {
      
    #' Adds location data to a SAFE data worksheet.
    #'
    #' This function adds location data to the rows of a SAFE data worksheet, 
	#' converting it into a \code{\link[sf]{sf} spatial features object. Rows
	#' are matched to values in a location_field: if there is only one location
	#' field, it will be used automatically; otherwise, the specific field must
	#' be provided.
	#' 
    #' @param obj An existing object of class \code{safedata}
	#' @param location_field The name of a location field in a \code{safedata} object.
	#' @param location_table An existing location table for a dataset, as generated by 
	#'    \code{\link{get_locations}}.
	#' @param gazetteer_info Should all the gazetteer fields be included in the 
	#'   returned \code{\link[sf]{sf}}  object. See \code{\link{load_gazetteer}} for details.
    #' @return A modified \code{safedata} 
    #' @seealso \code{\link{get_locations}}, \code{\link{load_gazetteer}}, 
	#'    \code{\link{load_location_aliases}}
	#' @examples
	#'    safedir <- system.file('example_data_dir', package='safedata')
	#'    set_safe_dir(safedir)
	#'    ant_abund <- load_safe_data(1400562, 'Ant-Psel')
	#'    ant_abund <- add_locations(ant_abund)
    #' @export
    
    if(! inherits(obj, 'safedata')){
        stop("add_locations requires an object of class 'safedata'")
    }
    
    # Get the location fields and check it
    obj_attr <- attr(obj, 'metadata')
    fields <- obj_attr$fields[[1]]
    location_fields <- with(fields, field_name[field_type == "Location"])
	
    if(length(location_fields) == 0){
        stop('Data frame does not contain location fields')
    } else if(is.null(location_field) && length(location_fields) == 1){
        location_field <- location_fields[1]
    } else if(is.null(location_field) && length(location_fields) > 1){
        stop('Data frame contains multiple location fields, specify location_field')
    } else if(! location_field %in% location_fields){
        stop(sprintf('%s is not a location field in the data frame', location_field))
    }
    
    # Load the taxonomy
    if(is.null(location_table)){
        location_table <- get_locations(obj_attr$safe_record_set)
    } else if(! inherits(location_table, 'safe_locations')){
        stop("'location_table' not a 'safe_locations' object created by 'get_locations'")
    }
    
	# This should never happen - would need a taxon field with no taxa worksheet
	if(is.null(location_table)){
		verbose_message('Dataset contains no location information.')
		return(obj)
	}
	
    # Match taxon names to the taxon and generate matching table
    idx <- match(obj[, location_field], rownames(location_table))
    
    if(any(is.na(idx))){
        stop('Not all location names found in locations table, contact package developers.')
    }
    
    location_table <- location_table[idx, ]
      
    # combine and copy across attributes (could create a method for cbind, but seems excessive!)
    ret <- sf::st_sf(data.frame(obj, location_table))
    attr(ret, 'metadata') <- obj_attr
    class(ret) <- c('safedata', class(ret))
    
    return(ret)
}
