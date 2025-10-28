
bgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("bgSel"), i18n$t("Background Extents:"),
                 #choices = list("Bounding box" = 'bb', 
                 #               "Minimum convex polygon" = 'mcp',
                 #               "Point buffers" = 'ptbuf'),
                 choiceNames = list(i18n$t("Bounding box"), i18n$t("Minimum convex polygon"), i18n$t("Point buffers")),
                 choiceValues = list('bb', 'mcp', 'ptbuf'),                 
                 selected='bb'),
    tags$div(title=i18n$t("Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position."),
             numericInput(ns("bgBuf"), label = i18n$t("Study region buffer distance (degree)"), value = 0.5, min = 0, step = 0.5))
  )
}

bgExtent_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$envs)) {
      rvs %>% writeLog(type = 'error', i18n$t("Before defining the background extent, obtain environmental data in component 3."))
      return()
      
    }
    if (nrow(rvs$occs) <= 2) {
      rvs %>% writeLog(type = 'error', i18n$t("Too few localities (<2) to create a background polygon."))
      return()
    }
    
    # record for RMD
    rvs$comp4.shp <- input$bgSel
    rvs$comp4.buf <- input$bgBuf
    
    # extract just coordinates
    occs.xy <- rvs$occs[c('longitude', 'latitude')]
    # make spatial pts object of original occs and preserve origID

    occs.sp <- sf::st_as_sf(occs.xy, coords = c("longitude", "latitude"), crs = 4326)
    occs.sp <- sf::as_Spatial(occs.sp)
    # occs.sp@data <- rvs$occs['occID']

    # generate background extent - one grid cell is added to perimeter of each shape
    # to ensure cells of points on border are included
    if (input$bgSel == 'bb') {
      xmin <- occs.sp@bbox[1]
      xmax <- occs.sp@bbox[3]
      ymin <- occs.sp@bbox[2]
      ymax <- occs.sp@bbox[4]
      bb <- matrix(c(xmin, xmin, xmax, xmax, xmin, ymin, ymax, ymax, ymin, ymin), ncol=2)

      bgExt_sf <- sf::st_sfc(sf::st_polygon(list(bb)), crs = 4326)
      msg <- i18n$t("Study extent: bounding box.")
    } else if (input$bgSel == 'mcp') {
      bgExt_sf <- mcp(occs.xy)
      msg <- i18n$t("Study extent: minimum convex polygon.")
    } else if (input$bgSel == 'ptbuf') {
      if (input$bgBuf == 0) {
        rvs %>% writeLog(type = 'error', i18n$t("Change buffer distance to positive or negative value."))
        return()
      }
      bgExt <- occs.sp
      msg <- i18n$t("Study extent: buffered points.")
    }
    
    if (input$bgBuf > 0) {
      if (input$bgSel == 'ptbuf'){
        eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
        occs.sp <- sf::st_as_sf(occs.xy, coords = c("longitude", "latitude"), crs = 4326)
        occs.sf <- sf::st_transform(occs.sp, crs = eckertIV)
        bgExt <- sf::st_buffer(occs.sf, dist = input$bgBuf * 111000) |>
          sf::st_union() |>
          sf::st_sf() |>
          sf::st_transform(crs = 4326) |>
          sf::as_Spatial()
      } else {
        bgExt_vect <- sf::st_transform(bgExt_sf , crs = 3100)
        bgExt <- sf::st_buffer(bgExt_vect, dist = input$bgBuf * 111000)|>
          sf::st_transform(crs = 4326) |>
          sf::as_Spatial()
      }
      rvs %>% writeLog(msg, i18n$t('Study extent buffered by'), input$bgBuf, i18n$t('degrees.'))
    } else {
      if (input$bgSel != 'ptbuf'){
        bgExt <- sf::as_Spatial(bgExt_sf)
      }
    }
    return(bgExt)
  })
}
