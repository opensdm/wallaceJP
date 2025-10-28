
bgGeodesic_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title=i18n$t("Point geodesic buffer in km"),
             numericInput(ns("bufferDist"), label = i18n$t("Point geodesic buffer in km"), value = 0))
  )
}

bgGeodesic_MOD <- function(input, output, session, rvs) {

  reactive({
    if (is.null(rvs$occs)) {
      rvs %>% writeLog(type = 'error', i18n$t("Before processing occurrences, obtain the data in component 1."))
      return()
      
    }
    # record for RMD
    rvs$bufDist <- input$bufferDist
    rvs$comp4.shp <- 'bufDist'
    rvs$comp4.bufDist <- input$bufferDist
    rvs$bgTgbs <- 'envmask'

    if (input$bufferDist == 0) {
      rvs %>% writeLog(type = 'error', i18n$t("Change buffer distance to positive or negative value."))
      return()
    }

    
    if (input$bufferDist > 0) {

      bgExt <- penvs_geodesic(rvs$occs,
                                     input$bufferDist,
                                     rvs,
                                     spN = rvs$spName)
      rvs %>% writeLog(i18n$t('Geodesic buffer for species calculated.'), input$bufferDist, i18n$t('km.'))
    }
    
    return(bgExt)
  })
}