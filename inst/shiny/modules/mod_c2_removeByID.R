
removeByID_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("removeID"), label=i18n$t("Enter the record ID to be removed"), value = 0)
  )
}

removeByID_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$occs)) {
      rvs %>% writeLog(type = 'error', i18n$t("Before processing occurrences, obtain the data in component 1."))
      return()
    }
    
    if (!(input$removeID %in% rvs$occs$occID)) {
      rvs %>% writeLog(type = 'error',i18n$t("Entered ID not found."))
      return()
    }
    
    # find row number relating to ID
    i <- which(input$removeID == rvs$occs$occID)  # find which row name corresponds to user selection for removal
    # remove the row
    occs.remID <- rvs$occs[-i,]
    # record all removed ids
    rvs$removedIDs <- c(rvs$removedIDs, input$removeID)
    
    rvs %>% writeLog(i18n$t("Removed occurrence with ID = "), input$removeID, 
                     i18n$t(". Updated data has n = "), nrow(rvs$occs), i18n$t(" records."))
    return(occs.remID)
  })
}
