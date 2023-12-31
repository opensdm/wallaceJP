envSimilarity_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

envSimilarity_MOD <- function(input, output, session, rvs) {
  
  reactive({
    req(rvs$envs, rvs$mods, rvs$predCur)
    
    if (is.null(rvs$projCur)) {
      rvs %>% writeLog(type = 'error', 'Project to new area or time first.')
      return()
    }
    if (is.null(rvs$polyPjXY)) {
      rvs %>% writeLog(type = 'error', "The polygon has not been drawn and finished. Please use the draw toolbar on the left-hand of the map to complete the polygon.")
      return()

    }
    
    
    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)
    bg.xy <- as.data.frame(rvs$bgPts)
    names(bg.xy) <- names(occs.xy)
    all.xy <- rbind(occs.xy, bg.xy)
    
    withProgress(message = "Generating MESS map...", {
      trainingVals <- raster::extract(rvs$envs, all.xy)
      pjMESS <- suppressWarnings(dismo::mess(rvs$projMsk, trainingVals))
      if (rvs$comp8.pj == 'area') {
        rvs %>% writeLog("Generated MESS map for present.")
      } else if (rvs$comp8.pj == 'time') {
        rvs %>% writeLog("Generated MESS map for", paste0('20', rvs$pjTimePar$year), 
                         "for GCM", GCMlookup[rvs$pjTimePar$gcm], 
                         "under RCP", as.numeric(rvs$pjTimePar$rcp)/10.0, ".")
      }
    })
    return(pjMESS)
  })
}
