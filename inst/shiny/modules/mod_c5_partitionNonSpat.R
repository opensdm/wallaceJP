
partNsp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partNspSel"), i18n$t("Options Available:"),
                choices = list("None selected" = '', "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand")),
    uiOutput(ns('kfoldsui'))
  )
}

partNsp_MOD <- function(input, output, session, rvs, occs, bgPts) {
  
  output$kfoldsui <- renderUI({
    ns <- session$ns
    if (input$partNspSel == "rand") {
      numericInput(ns("kfolds"), label = i18n$t("Number of Folds"), value = 2, min = 2)  
    }
  })
  
  
  reactive({
    if (is.null(rvs$bgMsk)) {
      rvs %>% writeLog(type = 'error', i18n$t("Before partitioning occurrences, mask your environmental variables by your background extent."))
      return()
      
    }
    if (input$partNspSel == '') {
      rvs %>% writeLog(type = 'error', i18n$t("Please select a partitioning option."))
      return()
    }
    
    # record for RMD
    rvs$comp5 <- input$partNspSel
    rvs$kfolds <- input$kfolds

    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)

    if (input$partNspSel == 'jack') {
      group.data <- ENMeval::get.jackknife(occs.xy, rvs$bgPts)
      rvs %>% writeLog(i18n$t("Occurrences partitioned by jackknife method."))
    } else if (input$partNspSel == 'rand') {
      group.data <- ENMeval::get.randomkfold(occs.xy, rvs$bgPts, input$kfolds)
      rvs %>% writeLog(i18n$t("Occurrences partitioned by random k-fold (k = "), input$kfolds, i18n$t(")."))
    }
      
    return(group.data)
  })
}
