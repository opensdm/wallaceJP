
bgMskAndSampleBiasPts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # bgTgbs は envProcSel == 'bgSel' のときのみ表示
    conditionalPanel("input.envProcSel == 'bgSel'",
                     radioButtons(ns("bgTgbs"), i18n$t("Modules Available:"),
                                  choiceNames = list(i18n$t("Target group background"), i18n$t("Envs Mask"), i18n$t("User-specified")),
                                  choiceValues = list('tgb', 'envmask', 'userbp'),                 
                                  selected='tgb'),
                     conditionalPanel(paste0("input['", ns("bgTgbs"), "']=='userbp'"),
                                      fileInput(ns("userbpCSV"), label = i18n$t("Upload background points (.csv)"),
                                                accept=c(".csv"))
                     )
    ),
    
    # 背景地点数の入力UIは、bgTgbs != 'userbp' または envProcSel != 'bgSel' のときに表示
    conditionalPanel(paste0("input['", ns("bgTgbs"), "'] != 'userbp' || input.envProcSel != 'bgSel'"),
                     numericInput(ns("bgPtsNum"), label = i18n$t("No. of background points"), value = 10000, min = 1, step = 1)
    )
  )
}

bgMskAndSampleBiasPts_MOD <- function(input, output, session, rvs) {
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
    rvs$comp4.userbp <- input$userbpCSV
    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)
   
    # mask envs by background extent
    withProgress(message = i18n$t("Processing environmental data..."), {
      bgCrop <- terra::crop(rvs$envs, rvs$bgShp)
      bgMask <- terra::mask(bgCrop, terra::vect(sf::st_as_sf(rvs$bgShp)))
    })
    rvs %>% writeLog(i18n$t("Environmental data masked."))
    # sample random background points
    withProgress(message = i18n$t("Generating background points..."), {
      if (input$bgTgbs == 'tgb' & rvs$c4_bgsel == 'bgSel' ) {
          if (length(rvs$userCSV) >1) {
            getgenus <- spocc::occ(query = rvs$spName, from = "gbif", limit=1)
            rvs$genusName <- getgenus$gbif$data[[names(getgenus$gbif$data)]]$genus
          }
          tgb_bbox <- sf::st_bbox(rvs$bgShp)
          taxonKey <- rgbif::name_backbone(name = rvs$genusName, rank = "genus")$usageKey
          tgb <- rgbif::occ_search(
                 taxonKey = taxonKey,
                 hasCoordinate = TRUE,
                 limit = input$bgPtsNum *4,
                 decimalLatitude = paste(tgb_bbox[[2]], tgb_bbox[[4]], sep = ","),
                 decimalLongitude = paste(tgb_bbox[[1]], tgb_bbox[[3]], sep = ","),
                 fields = c("decimalLongitude", "decimalLatitude")
             )
          tgb.xy <- tgb$data %>% dplyr::select(decimalLongitude, decimalLatitude) %>% dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))
          names(tgb.xy) <- c('longitude', 'latitude')
          tgb.xy <- as.data.frame(tgb.xy) 
	        tgb.xy <- tgb.xy[!duplicated(tgb.xy),]
          tgb.xy <- tgb.xy[sf::st_intersects(sf::st_as_sf(rvs$bgShp),sf::st_as_sf(tgb.xy,coords = c("longitude", "latitude"), crs = 4326))[[1]],]
          if(dim(tgb.xy)[1] >input$bgPtsNum){
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

      } else if (input$bgTgbs == 'userbp' & rvs$c4_bgsel == 'bgSel'){
          if (is.null(input$userbpCSV)) {
             rvs %>% writeLog(type = 'error', i18n$t("Bbackground points files not uploaded."))
             return()
          }
          names <- input$userbpCSV$name
          inPath <- input$userbpCSV$datapath
          pathdir <- dirname(inPath)
          pathfile <- basename(inPath)
          # get extensions of all input files
          exts <- sapply(strsplit(names, '\\.'), FUN=function(x) x[2])
          if (length(exts) == 1 && exts == 'csv') {

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
                   bgXY <- f.xy[sf::st_intersects(sf::st_as_sf(rvs$bgShp),sf::st_as_sf(f.xy,coords = c("longitude", "latitude"),crs = 4326))[[1]],]
                   rvs %>% writeLog(i18n$t("Background points uploaded (n ="), nrow(bgXY), i18n$t(")."))
                   shinyjs::enable("downloadMskPreds")
                   return(list(msk = bgMask, pts = bgXY))
             }
          }

      } else {
          if(rvs$c4_bgsel == 'bgGeodesic' | rvs$c4_bgsel == 'bgUser'){
            rvs$bgTgbs <- 'envmask'
          }
          #bgXY <- try(dismo::randomPoints(bgMask, input$bgPtsNum,))
          #bgXY <- terra::spatSample(bgMask, size = input$bgPtsNum, method = "random", as.point = TRUE, xy = TRUE)
          valid_cells <- which(!is.na(terra::values(bgMask$bio01)))
          if (input$bgPtsNum > length(valid_cells)) {
            sampled_cells <-valid_cells
          } else {
            sampled_cells <- sample(valid_cells, input$bgPtsNum)
          }
          bgXY <- terra::xyFromCell(bgMask$bio01, sampled_cells)
          # if(class(bgXY)[1] == "try-error"){
          #   rvs %>% writeLog(type = "error", i18n$t("generated random points < 0.1 requested number"))
          #   return()
          # }
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

      }
    })


  })
}
