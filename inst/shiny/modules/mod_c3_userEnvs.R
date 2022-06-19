
userEnvs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userEnvs"), label = i18n$t("Input rasters"), multiple = TRUE,
              accept = c(".tif", ".asc"))
  )
}

userEnvs_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$occs)) {
      rvs %>% writeLog(type = 'error', i18n$t("Before obtaining environmental variables, obtain occurrence data in component 1."))
      return()
      
    }
    if (is.null(input$userEnvs)) {
      rvs %>% writeLog(type = 'error', i18n$t("Raster files not uploaded."))
      return()
    }
    
    # record for RMD
    rvs$userEnvs <- input$userEnvs
    
    withProgress(message = i18n$t("Reading in rasters..."), {
      uenvs <- raster::stack(input$userEnvs$datapath)
      names(uenvs) <- fileNameNoExt(input$userEnvs$name)
    })
    
    rvs %>% writeLog(i18n$t("Environmental predictors: User input."))
    
    if (is.na(raster::crs(uenvs))) {
      rvs %>% writeLog(type = "warning", i18n$t("Input rasters have undefined coordinate reference system (CRS). Mapping functionality in components Visualize Model Results and Project Model will not work. If you wish to map rasters in these components, please define their projections and upload again. See guidance text in this module for more details."))
    }
    
    return(uenvs)
  })
}