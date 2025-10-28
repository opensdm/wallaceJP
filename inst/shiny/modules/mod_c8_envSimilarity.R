envSimilarity_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

envSimilarity_MOD <- function(input, output, session, rvs) {
  
  reactive({
    req(rvs$envs, rvs$mods, rvs$predCur)
    
    if (is.null(rvs$projCur)) {
      rvs %>% writeLog(type = 'error', i18n$t("Project to new area or time first."))
      return()
    }
    if (is.null(rvs$polyPjXY)) {
      rvs %>% writeLog(type = 'error', i18n$t("The polygon has not been drawn and finished. Please use the draw toolbar on the left-hand of the map to complete the polygon."))
      return()

    }
    
    
    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)
    bg.xy <- as.data.frame(rvs$bgPts)
    names(bg.xy) <- names(occs.xy)
    all.xy <- rbind(occs.xy, bg.xy)
    #browser()
    withProgress(message = i18n$t("Generating MESS map..."), {
      trainingVals <- terra::extract(rvs$envs, all.xy, ID=FALSE)
      pjMESS <- suppressWarnings(predicts::mess(rvs$projMsk, trainingVals))
      if (rvs$comp8.pj == 'area') {
        rvs %>% writeLog(i18n$t("Generated MESS map for present."))
      } else if (rvs$comp8.pj == 'time') {
        rvs %>% writeLog(i18n$t("Generated MESS map for"), rvs$pjTimePar$year, 
                         i18n$t("for GCM"), rvs$pjTimePar$gcm, 
                         i18n$t("under RCP"), rvs$pjTimePar$rcp)
      }
    })
    return(pjMESS)
  })
}
