
userBgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userBgShp"), label = i18n$t("Upload polygon with field order: longitude, latitude (.csv)"),
              accept=c(".csv", ".dbf", ".shx", ".shp"), multiple = TRUE),
    tags$div(title=i18n$t("Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position."),
             numericInput(ns("userBgBuf"), label = i18n$t("Study region buffer distance (degree)"), value = 0, min = 0, step = 0.5))
  )
}

userBgExtent_MOD <- function(input, output, session, rvs) {
  userBgShp <- reactive({
    if (is.null(rvs$envs)) {
      rvs %>% writeLog(type = 'error', i18n$t("Environmental variables missing. Obtain them in component 3."))
      return()
      
    }
    if (is.null(input$userBgShp)) {
      rvs %>% writeLog(type = 'error', i18n$t("Background extent files not uploaded."))
      return()
    }
    
    # record for RMD
    rvs$comp4.buf <- input$userBgBuf
    
    names <- input$userBgShp$name
    inPath <- input$userBgShp$datapath
    pathdir <- dirname(inPath)
    pathfile <- basename(inPath)
    # get extensions of all input files
    exts <- sapply(strsplit(names, '\\.'), FUN=function(x) x[2])

    if (length(exts) == 1 && exts == 'csv') {
      # record for RMD
      rvs$comp4.shp <- 'csv'
      f <- read.csv(inPath, header = TRUE)

      # bgExt <- sf::as_Spatial(sf::st_sfc(sf::st_polygon(list(as.matrix(f))), crs = 4326))
      bgExt <- sf::st_sfc(sf::st_polygon(list(as.matrix(f))), crs = 4326)
    } else if ('shp' %in% exts) {
      if (length(exts) < 3) {
        rvs %>% writeLog(type = 'error', i18n$t("If entering a shapefile, please select all the following files: .shp, .shx, .dbf."))
        return()
      }
      #file.rename(inPath, file.path(pathdir, names))
      # get index of .shp
      i <- which(exts == 'shp')
      shpName <- strsplit(names[i], '\\.')[[1]][1]
      if (!file.exists(file.path(pathdir, names)[i])) {
        file.rename(inPath, file.path(pathdir, names))
      }
      
      # record for RMD
      rvs$comp4.shp <- 'shp'
      rvs$bgUserShpPar <- list(dsn=pathdir[i], layer=shpName)
      
      # read in shapefile and extract coords
      bgExt <- terra::vect(paste0(pathdir[i],"/",shpName,".shp"))
    } else {
      rvs %>% writeLog(type = 'error', i18n$t("Please enter either a CSV file of vertex coordinates or shapefile (.shp, .shx, .dbf)."))
      return()
    }
    rvs %>% writeLog(i18n$t("Study extent: user-defined polygon."))
    return(bgExt)
  })
  
  bufBg <- reactive({
    bufWid <- input$userBgBuf
    bgExt_sf <- userBgShp()
    if (bufWid > 0) {
      if ("SpatVector" %in% class(bgExt_sf)) {
        bgExt <- terra::buffer(bgExt_sf, width = bufWid)
      } else {
        bgExt_vect <- sf::st_transform(bgExt_sf , crs = 3100)
        bgExt <- sf::st_buffer(bgExt_vect, dist = bufWid * 111000)|>
          sf::st_transform(crs = 4326)
      }
      rvs %>% writeLog(i18n$t('Study extent buffered by'), bufWid, i18n$t('degrees.'))
    } else {
      bgExt <- bgExt_sf
    }
    
    bgExt <- sf::st_as_sf(bgExt)
    bgExt <- sf::as_Spatial(bgExt)
    return(bgExt)
  })
  
  return(bufBg)
}