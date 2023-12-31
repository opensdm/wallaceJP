
mapPredsMaxent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Please see guidance for an explanation of different Maxent output types.',
             radioButtons(ns('predType'), label = "Prediction output",
                choices = list("raw", "logistic", "cloglog"), selected = "raw", inline = TRUE)),
    threshPred_UI(ns('threshPred'))
  )
}

mapPredsMaxent_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$mods)) {
      rvs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }
    
    # record for RMD
    rvs$comp7.type <- input$predType
    rvs$comp7 <- c(rvs$comp7, 'map')
    
    # initially pick raw prediction
    predSel <- rvs$modPreds[[rvs$modSel]]
    raster::crs(predSel) <- raster::crs(rvs$bgMsk)
    names(predSel) <- paste0(rvs$modSel, '_raw')
    
    if (is.na(raster::crs(predSel))) {
      rvs %>% writeLog(type = "error", "Model prediction raster has undefined coordinate reference system (CRS), and thus cannot be mapped. This is likely due to undefined CRS's for input rasters. Please see guidance text for module 'User-specified Environmental Data' in component 'Obtain Environmental Data' for more details.")
      return()
    }
    
    # argument for predict function
    pargs <- rvs$comp7.type
    
    if (input$predType == 'logistic') {
      # Generate logistic predictions for each model
      if (is.null(rvs$modPredsLog)) {
        withProgress(message = "Generating logistic predictions...", {
          logPredsList <- if (rvs$algMaxent == "maxnet") {
            sapply(rvs$mods, function(x) predictMaxnet(x, rvs$bgMsk, type=pargs, 
                                                                       clamp = rvs$clamp))
          } else if (rvs$algMaxent == "maxent.jar") {
            sapply(rvs$mods, function(x) dismo::predict(x, rvs$bgMsk, 
                                                        args = paste0("outputformat=", 
                                                                      rvs$comp7.type)))
          }
          rvs$modPredsLog <- raster::stack(logPredsList)
          names(rvs$modPredsLog) <- names(rvs$modPreds)
        })
      }
      predSel <- rvs$modPredsLog[[rvs$modSel]]
      raster::crs(predSel) <- raster::crs(rvs$bgMsk)
      names(predSel) <- paste0(rvs$modSel, '_log')
    } else if (input$predType == 'cloglog') {
      # Generate cloglog predictions for each model
      if (is.null(rvs$modPredsCLL)) {
        withProgress(message = "Generating cloglog predictions...", {
          cllPredsList <- if (rvs$algMaxent == "maxnet") {
            sapply(rvs$mods, function(x) predictMaxnet(x, rvs$bgMsk, type = pargs, 
                                                                       clamp = rvs$clamp))
          } else if (rvs$algMaxent == "maxent.jar") {
            sapply(rvs$mods, function(x) dismo::predict(x, rvs$bgMsk, 
                                                        args = paste0("outputformat=", 
                                                                      rvs$comp7.type)))
          }
          rvs$modPredsCLL <- raster::stack(cllPredsList)
          names(rvs$modPredsCLL) <- names(rvs$modPreds)
        })  
      }
      predSel <- rvs$modPredsCLL[[rvs$modSel]]
      raster::crs(predSel) <- raster::crs(rvs$bgMsk)
      names(predSel) <- paste0(rvs$modSel, '_cll')
    }
    
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    predSel.thr.call <- callModule(threshPred_MOD, "threshPred", predSel)
    predSel.thr <- predSel.thr.call()
    pjPred <- predSel.thr$pred
    rvs$comp7.thr <- predSel.thr$thresh
    
    # write to log box
    if (rvs$clamp == T | rvs$algMaxent == "maxent.jar") {
      rvs %>% writeLog("Maxent", input$predType, "clamped model prediction plotted.")
    } else if (rvs$clamp == F) {
      rvs %>% writeLog("Maxent", input$predType, "unclamped model prediction plotted.")
    }
    
    
    return(pjPred)
  })
}
