
userOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userCSV"), label = i18n$t("Upload Occurrence CSV"))
  )
}

userOccs_MOD <- function(input, output, session, rvs) {
  
  readOccsCSV <- reactive({
    req(input$userCSV)
    
    # make occDB record NULL to keep track of where occurrences are coming from
    rvs$occDB <- NULL
    # record for RMD
    rvs$userCSV <- input$userCSV
    
    csv <- read.csv(input$userCSV$datapath)
    
    spName <- trimws(as.character(csv$name[1]))
    
    if (!all(c('name', 'longitude', 'latitude') %in% names(csv))) {
      rvs %>% writeLog(type = "error", i18n$t("Please input CSV file with columns 'name', 'longitude', 'latitude'."))
      return()
    }
    
    
    # subset to just records with first species name, and non-NA latitude and longitude
    uoccs <- csv %>% 
      dplyr::filter(name == spName) %>%
      dplyr::filter(!is.na(latitude) & !is.na(longitude))
    
    # remove duplicates
    uoccs.dups <- duplicated(uoccs %>% dplyr::select(longitude, latitude))
    uoccs <- uoccs[!uoccs.dups,]
      
    if (nrow(uoccs) == 0) {
      rvs %>% writeLog(type = 'warning', i18n$t('No records with coordinates found in'), 
                        input$userCSV$name, i18n$t("for"), spName, i18n$t("."))
      return()
    }
    
    rvs %>% writeLog(i18n$t("User-specified CSV file"), input$userCSV$name, i18n$t("with total of"), 
                      nrow(uoccs), i18n$t("records with coordinates was uploaded."))
    
    for (col in c("year", "institutionCode", "catalogNumber", "basisOfRecord", "country", "stateProvince",
                  "locality", "elevation")) {  # add all cols to match origOccs if not already there
      if (!(col %in% names(uoccs))) uoccs[,col] <- NA
    }
    
    uoccs$occID <- row.names(uoccs)  # add col for IDs
    uoccs$pop <- unlist(apply(uoccs, 1, popUpContent))  # add col for map marker popup text
    
    return(uoccs)
  })
  
  return(readOccsCSV)
}
