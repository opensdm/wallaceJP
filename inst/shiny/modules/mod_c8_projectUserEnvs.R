
projectUserEnvs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("projectUserEnvs"), label = i18n$t("Input rasters"), multiple = TRUE,
              accept = c(".tif", ".asc")),
    threshPred_UI(ns('threshPred'))
  )
}

projectUserEnvs_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$predCur)) {
      rvs %>% writeLog(type = 'error', i18n$t("Calculate a model prediction in component 7 before projecting."))
      return()

    }
    if (is.null(rvs$polyPjXY) | identical(rvs$polySelXY, rvs$polyPjXY)) {
      rvs %>% writeLog(type = 'error', i18n$t("The polygon has not been drawn and finished. Please use the draw toolbar on the left-hand of the map to complete the polygon."))
      return()
    }
    if (is.null(input$projectUserEnvs)) {
      rvs %>% writeLog(type = 'error', i18n$t("Raster files not uploaded."))
      return()
    }
    # Check the number of selected files
    if (length(input$projectUserEnvs$name) !=
        length(rvs$userEnvs$name)) {
      rvs %>%
        writeLog(type = 'error', "Number of files are not the same that the ",
                 "enviromental variables",length(rvs$userEnvs$name))
      return()
    }
    # Check if the filesnames are the same that envs()
    if (!identical(tools::file_path_sans_ext(sort(input$projectUserEnvs$name)),
                   sort(fileNameNoExt(rvs$userEnvs$name)))) {
      rvs %>%
        writeLog(type = 'error',
                 paste0("Raster files don't have same names. You must name your",
                        " files as: "),
                 paste(rvs$userEnvs$name,
                          collapse = ", "), ".")
      return()
    }

    # record for RMD
    rvs$projectUserEnvs <- input$projectUserEnvs
    
    withProgress(message = i18n$t("Reading in rasters..."), {
      penvs <- raster::stack(input$projectUserEnvs$datapath)
      names(penvs) <- fileNameNoExt(input$projectUserEnvs$name)
    })

    # Check if the filesresolutions are the same that envs()
    if (!identical(round(raster::res(rvs$envs)[1]*60,1),
                   round(raster::res(penvs)[1]*60,1))) {
      rvs %>%
        writeLog(type = 'error',"Raster files don't have same resolutions.")
      return()
    }
    
    rvs %>% writeLog(i18n$t("Environmental predictors: User input."))
    
    if (is.na(raster::crs(penvs))) {
      rvs %>% writeLog(type = "warning", i18n$t("Input rasters have undefined coordinate reference system (CRS). Mapping functionality in components Visualize Model Results and Project Model will not work. If you wish to map rasters in these components, please define their projections and upload again. See guidance text in this module for more details."))
    }
    
        # create new spatial polygon from coordinates
    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rvs$polyPjXY)), ID=rvs$polyPjID)))  
    
    # concatanate coords to a single character
    xy.round <- round(rvs$polyPjXY, digits = 2)
    coordsChar <- paste(apply(xy.round, 1, function(b) paste0('(',paste(b, collapse=', '),')')),
                        collapse=', ')  
    if (rvs$comp6 == 'bioclim') {
      rvs %>% writeLog(i18n$t('New time projection for BIOCLIM model with extent coordinates:'), coordsChar)
    } else if (rvs$comp6 == 'maxent') {
      if (rvs$clamp == T | rvs$algMaxent == "maxent.jar") {
        rvs %>% writeLog(i18n$t('New time projection for clamped model'), rvs$modSel, i18n$t('with extent coordinates:'),
                         coordsChar)
      } else if (rvs$clamp == F) {
        rvs %>% writeLog(i18n$t('New time projection for unclamped model'), rvs$modSel, i18n$t('with extent coordinates:'),
                         coordsChar)
      }
    }
    
    
    withProgress(message = i18n$t("Clipping environmental data to current extent..."), {
      pjtMsk <- raster::crop(penvs, newPoly)
      pjtMsk <- raster::mask(pjtMsk, newPoly)
    })
    
    modCur <- rvs$mods[[rvs$modSel]]
    
    withProgress(message = (i18n$t("Projecting to new time...")), {
      if (rvs$comp6 == 'bioclim') {
        modProjTime <- dismo::predict(modCur, pjtMsk, useC = FALSE)
      } else if (rvs$comp6 == 'maxent') {
        if (rvs$algMaxent == "maxnet") {
          if (rvs$comp7.type == "raw") {pargs <- "exponential"} else {pargs <- rvs$comp7.type}
          modProjTime <- predictMaxnet(modCur, pjtMsk, type = pargs, clamp = rvs$clamp)
        } else if (rvs$algMaxent == "maxent.jar") {
          pargs <- paste0("outputformat=", rvs$comp7.type)
          modProjTime <- dismo::predict(modCur, pjtMsk, args = pargs)
        }
      }
      
      modProjTime.thr.call <- callModule(threshPred_MOD, "threshPred", modProjTime)
      modProjTime.thr <- modProjTime.thr.call()
      pjPred <- modProjTime.thr$pred
      rvs$comp8.thr <- modProjTime.thr$thresh
      rvs %>% writeLog(i18n$t("Projected to"), paste0('20', input$selTime), 
                       i18n$t("for GCM"), GCMlookup[input$selGCM], 
                       i18n$t("under RCP"), as.numeric(input$selRCP)/10.0, ".")
    })
    
    return(list(pjMsk=pjtMsk, pjPred=pjPred))
  })
}