
bgMskAndSamplePts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bgPtsNum"), label = i18n$t("No. of background points"), value = 10000, min = 1, step = 1)
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
    
    # mask envs by background extent
    withProgress(message = i18n$t("Processing environmental data..."), {
      bgCrop <- raster::crop(rvs$envs, rvs$bgShp)
      bgMask <- raster::mask(bgCrop, rvs$bgShp)
    })
    rvs %>% writeLog(i18n$t("Environmental data masked."))
    # sample random background points
    withProgress(message = i18n$t("Generating background points..."), {
      bgXY <- dismo::randomPoints(bgMask, input$bgPtsNum)
    })
    bg.prop <- round(nrow(bgXY)/input$bgPtsNum, digits = 2)
    if(bg.prop == 1) {
      rvs %>% writeLog(i18n$t("Random background points sampled (n ="), input$bgPtsNum, i18n$t(")."))
    } else {
      rvs %>% writeLog(i18n$t("Random background points requested (n ="), input$bgPtsNum, 
                       i18n$t("), but only "), 100*bg.prop, i18n$t("% of points (n = "), nrow(bgXY), i18n$t(") were able to be sampled."))
    }
    shinyjs::enable("downloadMskPreds")
    return(list(msk = bgMask, pts = bgXY))
  })
}
