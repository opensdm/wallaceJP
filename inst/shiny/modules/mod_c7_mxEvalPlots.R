
mxEvalPlots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('mxEvalSel'), label = i18n$t("Select evaluation statistic"),
                choices = list("Select Stat..." = '', 
                               "average AUC validation" = 'auc.val', 
                               "average AUC diff" = 'auc.diff', 
                               "average OR mtp" = 'or.mtp',
                               "average OR 10%" = 'or.10p', 
                               "delta AICc" = 'delta.AICc'), 
                selected = 'auc.val'),
    HTML('<hr>'),
    strong(i18n$t("Download Maxent evaluation plot (.png)")), br(), br(),
    downloadButton(ns('dlMxEvalPlot'), i18n$t("Download"))
  )
}

mxEvalPlots_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$mods)) {
      rvs %>% writeLog(type = 'error', i18n$t("Models must first be run in component 6."))
      return()
    }
    if (is.null(input$mxEvalSel)) {
      rvs %>% writeLog(type = 'error', i18n$t("No statistic selected for plotting."))
      return()
    }
    
    # record for RMD
    rvs$mxEvalSel <- input$mxEvalSel
    rvs$comp7 <- isolate(c(rvs$comp7, 'mxEval'))

    # handle downloads for Maxent Evaluation Plots png
    output$dlMxEvalPlot <- downloadHandler(
      filename = function() {paste0(spName(), "_",rvs$mxEvalSel,"_EvalPlot.png")},
      content = function(file) {
        png(file)
        plot(ENMeval::evalplot.stats(new("ENMevaluation", results = rvs$modRes), 
                                     rvs$mxEvalSel, "rm", "fc"))
        dev.off()
      }
    )
    plot(ENMeval::evalplot.stats(new("ENMevaluation", results = rvs$modRes), 
                                 rvs$mxEvalSel, "rm", "fc"))
  })
}
