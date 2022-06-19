
thinOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title=i18n$t("The minimum distance between occurrence locations (nearest neighbor distance) in km for resulting thinned dataset. Ideally based on species biology (e.g., home-range size)."),
             numericInput(ns("thinDist"), label = i18n$t("Thinning distance (km)"), value = 0))
  )
}

thinOccs_MOD <- function(input, output, session, rvs) {

  doThin <- reactive({
    if (is.null(rvs$occs)) {
      rvs %>% writeLog(type = 'error', i18n$t("Before processing occurrences, obtain the data in component 1."))
      return()
      
    }
    
    if (input$thinDist <= 0) {
      rvs %>% writeLog(type = "error", i18n$t("Assign positive distance to thinning parameter."))
      return()
    }
    
    # record for RMD
    rvs$thinDist <- input$thinDist
    
    withProgress(message = i18n$t("Spatially Thinning Localities..."), {  # start progress bar
      output <- spThin::thin(rvs$occs, 'latitude', 'longitude', 'name', thin.par = input$thinDist,
                             reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE,
                             verbose = FALSE)
      
      # pull thinned dataset with max records, not just the first in the list
      maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
      maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]  # if more than one max, pick first
      occs.thin <- rvs$occs[as.numeric(rownames(maxThin)),]
      # if (!is.null(values$inFile)) {
      #   thinned.inFile <- values$inFile[as.numeric(rownames(output[[1]])),]
      # }
    })
    
    rvs %>% writeLog(i18n$t('Total records thinned to ['), nrow(occs.thin), i18n$t('] localities.'))
    
    return(occs.thin)
  })

  return(doThin)
}