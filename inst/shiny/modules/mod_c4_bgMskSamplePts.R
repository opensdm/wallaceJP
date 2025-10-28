
bgMskAndSamplePts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("bgTgbs"), i18n$t("Modules Available:"),
                 choiceNames = list(i18n$t("Target group"), i18n$t("Envs Mask"), i18n$t("User-specified")),
                 choiceValues = list('tgb', 'envmask', 'userbg'),                 
                 selected='tgb'),
    conditionalPanel(paste0("input['", ns("bgTgbs"), "']=='userbg'"),
             tags$div(
                    fileInput(ns("userbgCSV"), label = i18n$t("Upload Target group background points with field order: longitude, latitude (.csv)"),
                              accept=c(".csv"))
            )
    ),
    conditionalPanel(paste0("input['", ns("bgTgbs"), "']!='userbg'"),
            tags$div(
                    numericInput(ns("bgPtsNum"), label = i18n$t("No. of background points"), value = 10000, min = 1, step = 1)
            )
    )
  )
}

bgMskAndSamplePts_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$bgShp)) {
      rvs %>% writeLog(type = 'error', i18n$t("Before sampling background points, define the background extent."))
      return()
      
    }
    
    if (input$bgPtsNum < 1) {
      rvs %>% writeLog(type = 'warning',
                       i18n$t("Enter a non-zero number of background points."))
      return()
    }
    
    # record for RMD
    rvs$bgPtsNum <- input$bgPtsNum
    rvs$bgTgbs <- input$bgTgbs
    rvs$comp4.userbg <- input$userbgCSV
    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)

    # mask envs by background extent
    withProgress(message = i18n$t("Processing environmental data..."), {
      bgCrop <- raster::crop(rvs$envs, rvs$bgShp)
      bgMask <- raster::mask(bgCrop, rvs$bgShp)
    })
    rvs %>% writeLog(i18n$t("Environmental data masked."))
    # sample random background points
    withProgress(message = i18n$t("Generating background points..."), {
      if (input$bgTgbs == 'tgb') {
          #print(rvs)
          if (length(rvs$userCSV) >1) {
            getgenus <- spocc::occ(query = rvs$spName, from = "gbif", limit=1)
            rvs$genusName <- getgenus$gbif$data[[names(getgenus$gbif$data)]]$genus
          }
          tgb <- spocc::occ(query = rvs$genusName, from = "gbif", geometry = sp::bbox(rvs$bgShp),limit= input$bgPtsNum *4)
          tgb.xy <- tgb[["gbif"]]$data[[formatSpName(rvs$genusName)]]
          tgb.xy <- tgb.xy[c('longitude', 'latitude')]
          tgb.xy <- as.data.frame(tgb.xy) 
	  tgb.xy <- tgb.xy[!duplicated(tgb.xy),]
          #tgb.xy <- dplyr::setdiff(tgb.xy,occs.xy)
          tgb.xy <- tgb.xy[sf::st_intersects(sf::st_as_sf(rvs$bgShp),sf::st_as_sf(tgb.xy,coords = c("longitude", "latitude")))[[1]],]
          if(input$bgPtsNum < dim(tgb.xy)[1]){
             bgXY <- dplyr::sample_n(tbl = tgb.xy,size = input$bgPtsNum)
          } else {
             bgXY <- tgb.xy
          }
          bg.prop <- round(nrow(bgXY)/input$bgPtsNum, digits = 2)
          if(bg.prop == 1) {
              rvs %>% writeLog(i18n$t("Target group background points sampled (n ="), input$bgPtsNum, i18n$t(")."))
          } else {
              rvs %>% writeLog(i18n$t("Target group background points requested (n ="), input$bgPtsNum, 
                       i18n$t("), but only "), 100*bg.prop, i18n$t("% of points (n = "), nrow(bgXY), i18n$t(") were able to be sampled."))
          }
          shinyjs::enable("downloadMskPreds")
          return(list(msk = bgMask, pts = bgXY))

      } else if (input$bgTgbs == 'envmask'){
          bgXY <- dismo::randomPoints(bgMask, input$bgPtsNum)
          bgXY <- as.data.frame(bgXY) 
          colnames(bgXY) <- c("longitude", "latitude")
          bg.prop <- round(nrow(bgXY)/input$bgPtsNum, digits = 2)
          if(bg.prop == 1) {
              rvs %>% writeLog(i18n$t("Random background points sampled (n ="), input$bgPtsNum, i18n$t(")."))
          } else {
              rvs %>% writeLog(i18n$t("Random background points requested (n ="), input$bgPtsNum, 
                       i18n$t("), but only "), 100*bg.prop, i18n$t("% of points (n = "), nrow(bgXY), i18n$t(") were able to be sampled."))
          }
          shinyjs::enable("downloadMskPreds")
          return(list(msk = bgMask, pts = bgXY))
      } else {
          if (is.null(input$userbgCSV)) {
             rvs %>% writeLog(type = 'error', i18n$t("Target group background files not uploaded."))
             return()
          }
          names <- input$userbgCSV$name
          inPath <- input$userbgCSV$datapath
          pathdir <- dirname(inPath)
          pathfile <- basename(inPath)
          # get extensions of all input files
          exts <- sapply(strsplit(names, '\\.'), FUN=function(x) x[2])
          if (length(exts) == 1 & exts == 'csv') {

              f.xy <- read.csv(inPath, header = TRUE)
              if (!all(c('longitude', 'latitude') %in% names(f.xy))) {
                   rvs %>% writeLog(type = "error", i18n$t("Please input CSV file with columns 'longitude', 'latitude'."))
                   return()
              } else {
                   f.xy$latitude <- as.numeric(f.xy$latitude)
                   f.xy$longitude <- as.numeric(f.xy$longitude)
                   f.xy <- f.xy[c('longitude', 'latitude')]
                   f.xy <- as.data.frame(f.xy)
                   colnames(f.xy) <- c("longitude", "latitude")
                   f.xy <- f.xy[!is.na(f.xy$longitude) & !is.na(f.xy$latitude),]
                   bgXY <- f.xy[sf::st_intersects(sf::st_as_sf(rvs$bgShp),sf::st_as_sf(f.xy,coords = c("longitude", "latitude")))[[1]],]
                   rvs %>% writeLog(i18n$t("Target group background points uploaded (n ="), nrow(bgXY), i18n$t(")."))
                   shinyjs::enable("downloadMskPreds")
                   return(list(msk = bgMask, pts = bgXY))
             }
          }

      }
    })


  })
}
