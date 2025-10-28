projectTime_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("selTime"), label = i18n$t("Select time period"),
                choices = list("Select period" = "",
                               "2021-2040" = "2021-2040",
                               "2041-2060" = "2041-2060",
                               "2061-2080" = "2061-2080")),
    selectInput(ns("selGCM"), label = i18n$t("Select global circulation model"),
                choices = list("Select GCM" = "",
                               "ACCESS-CM2" = "ACCESS-CM2",
                               "ACCESS-ESM1-5" = "ACCESS-ESM1-5",
                               "AWI-CM-1-1-MR" = "AWI-CM-1-1-MR",
                               "BCC-CSM2-MR" = "BCC-CSM2-MR",
                               "CanESM5" = "CanESM5",
                               "CanESM5-CanOE" = "CanESM5-CanOE",
                               "CMCC-ESM2" = "CMCC-ESM2",
                               "CNRM-CM6-1" = "CNRM-CM6-1",
                               "CNRM-CM6-1-HR" = "CNRM-CM6-1-HR",
                               "CNRM-ESM2-1" = "CNRM-ESM2-1",
                               "EC-Earth3-Veg" = "EC-Earth3-Veg",
                               "EC-Earth3-Veg-LR" = "EC-Earth3-Veg-LR",
                               "GISS-E2-1-G" = "GISS-E2-1-G",
                               "GISS-E2-1-H" = "GISS-E2-1-H",
                               "INM-CM4-8" = "INM-CM4-8",
                               "INM-CM5-0" = "INM-CM5-0",
                               "IPSL-CM6A-LR" = "IPSL-CM6A-LR",
                               "MIROC-ES2L" = "MIROC-ES2L",
                               "MIROC6" = "MIROC6",
                               "MPI-ESM1-2-HR" = "MPI-ESM1-2-HR",
                               "MPI-ESM1-2-LR" = "MPI-ESM1-2-LR",
                               "MRI-ESM2-0" = "MRI-ESM2-0",
                               "UKESM1-0-LL" = "UKESM1-0-LL")),
    selectInput(ns('selRCP'), label = i18n$t("Select RCP"),
                choices = list("Select RCP" = "",
                               "126" = "126",
                               "245" = "245",
                               "370" = "370",
                               "585" = "585")),
    threshPred_UI(ns('threshPred'))
  )
}

projectTime_MOD <- function(input, output, session, rvs) {
  
  reactive({
    if (is.null(rvs$predCur)) {
      rvs %>% writeLog(type = 'error', i18n$t("Calculate a model prediction in component 7 before projecting."))
      return()

    }
    if (is.null(rvs$polyPjXY) | identical(rvs$polySelXY, rvs$polyPjXY)) {
      rvs %>% writeLog(type = 'error', i18n$t("The polygon has not been drawn and finished. Please use the draw toolbar on the left-hand of the map to complete the polygon."))
      return()
    }
    

    # record for RMD
    rvs$pjTimePar <- list(rcp=input$selRCP, gcm=input$selGCM, year=input$selTime)
    

    if (is.null(rvs$polyPjXY)) {
      rvs %>% writeLog(type = 'error', i18n$t("Select projection extent first."))
      return()
    }
    envsRes <- terra::res(rvs$envs)[1]
    if (envsRes < 0.05) {
    #  rvs %>% writeLog(type = 'error', i18n$t("Project to New Time currently only available with resolutions >30 arc seconds."))
    #  return()
       res = round(envsRes * 60 ,1)
    } else {
       res = as.integer(envsRes * 60)
    }
    
    # code taken from dismo getData() function to catch if user is trying to 
    # download a missing combo of gcm / rcp
    # gcms <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 
    #           'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
    # rcps <- c(26, 45, 60, 85)
    # m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    #               1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,
    #               0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
    # i <- m[which(input$selGCM == gcms), which(input$selRCP == rcps)]
    # if (!i) {
    #   rvs %>% writeLog(type = 'error', i18n$t("This combination of GCM and RCP is not available. Please make a different selection."))
    #   return()
    # }
    
    withProgress(message = paste(i18n$t("Retrieving WorldClim data for"), input$selTime, input$selRCP, "..."), {
      # projTimeEnvs <- getDataEx('CMIP5', var = "bio", res = res,
      #                                 rcp = input$selRCP, model = input$selGCM, year = input$selTime)
      projTimeEnvs <- tryCatch(expr = geodata::cmip6_world(model = input$selGCM,
                                                           ssp = input$selRCP,
                                                           time = input$selTime,
                                                           var = "bio",
                                                           res = round(envsRes * 60, 1),
                                                           path = getwd()),
                               error= function(e) NULL)
      names(projTimeEnvs) <- paste0('bio', c(paste0('0',1:9), 10:19))
      # in case user subsetted bioclims
      projTimeEnvs <- projTimeEnvs[[rvs$bcSels]]
    })
    
    # create new spatial polygon from coordinates
    # newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rvs$polyPjXY)), ID=rvs$polyPjID)))  
    newPoly <- sf::as_Spatial(sf::st_sfc(sf::st_polygon(list(rvs$polyPjXY)), crs = 4326))
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
      # pjtMsk <- raster::crop(projTimeEnvs, newPoly)
      # pjtMsk <- raster::mask(pjtMsk, newPoly)
      pjtMsk <- terra::crop(projTimeEnvs, newPoly)
      pjtMsk <- terra::mask(pjtMsk, terra::vect(sf::st_as_sf(newPoly)))
    })
    
    modCur <- rvs$mods[[rvs$modSel]]
    
    withProgress(message = (i18n$t("Projecting to new time...")), {
      if (rvs$comp6 == 'bioclim') {
        # modProjTime <- dismo::predict(modCur, pjtMsk, useC = FALSE)
        modProjTime <- predicts::predict(pjtMsk, modCur)
      } else if (rvs$comp6 == 'maxent') {
        if (rvs$algMaxent == "maxnet") {
          if (rvs$comp7.type == "raw") {pargs <- "exponential"} else {pargs <- rvs$comp7.type}
          modProjTime <- predictMaxnet(modCur, pjtMsk, type = pargs, clamp = rvs$clamp)
        } else if (rvs$algMaxent == "maxent.jar") {
          pargs <- paste0("outputformat=", rvs$comp7.type)
          #modProjTime <- dismo::predict(modCur, pjtMsk, args = pargs)
          modProjTime <- predicts::predict(pjtMsk, modCur, args = pargs, clamp = rvs$clamp, na.rm = TRUE)
        }
      }
      
      modProjTime.thr.call <- callModule(threshPred_MOD, "threshPred", modProjTime)
      modProjTime.thr <- modProjTime.thr.call()
      pjPred <- modProjTime.thr$pred
      rvs$comp8.thr <- modProjTime.thr$thresh
      rvs %>% writeLog(i18n$t("Projected to"), input$selTime, 
                       i18n$t("for GCM"), input$selGCM, 
                       i18n$t("under RCP"), input$selRCP)
    })
    
    return(list(pjMsk=pjtMsk, pjPred=pjPred))
  })
}
