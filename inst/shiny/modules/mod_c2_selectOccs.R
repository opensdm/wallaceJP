selectOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

selectOccs_MOD <- function(input, output, session, rvs) {
  
  reactive({
    if (is.null(rvs$occs)) {
      rvs %>% writeLog(type = 'error', i18n$t("Before processing occurrences, obtain the data in component 1."))
      return()

    }
    if (is.null(rvs$polySelXY)) {
      rvs %>% writeLog(type = 'error', i18n$t("The polygon has not been finished. Please press 'Finish' on the map toolbar. then the 'Select Occurrences' button."))
      return()

    }
    
    
    occs.xy <- rvs$occs[c('longitude', 'latitude')]
    
    # make spatial pts object of original occs and preserve origID
    #pts <- sp::SpatialPointsDataFrame(occs.xy, data=rvs$occs['occID'])
    pts <- sf::st_as_sf(occs.xy, coords = c("longitude", "latitude"), crs = 4326)
    
    pts$occID <- rvs$occs$occID
    
    #newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rvs$polySelXY)), ID=rvs$polySelID)))  # create new polygon from coords

    poly_matrix <- list(rvs$polySelXY)

    newPoly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(poly_matrix)),
                     ID = rvs$polySelID,
                     crs = 4326
               )


    #intersect <- sp::over(pts, newPoly)
    intersect <- sf::st_disjoint(pts, newPoly, sparse = FALSE)

    #ptRemIndex <- which(is.na(intersect))

    ptRemIndex <- which(intersect)
    
    if (length(ptRemIndex) > 0) {
      ptRemIndex <- as.numeric(ptRemIndex)

      remIDs <- as.numeric(pts[ptRemIndex,]$occID)
      
      occs.sel <- rvs$occs[-ptRemIndex,]
      
      rvs %>% writeLog(i18n$t("Removing occurrences with occID = "), remIDs, 
                       i18n$t(". Updated data has n = "), nrow(occs.sel), i18n$t(" records."))      
      return(occs.sel)
    } else {
      rvs %>% writeLog(i18n$t("Please select a subset of points to retain in the analysis.")) 
      polySelX <- polySelY <- NULL
      return(rvs$occs)
    }
  })
}
