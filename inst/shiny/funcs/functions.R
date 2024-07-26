library(shiny)
library(shiny.i18n)

language_value <- "ja"
i18n <- Translator$new(translation_csvs_path = "translation")
i18n$set_translation_language(language_value)

## -------------------------------------------------------------------- ##
## Define functions
## -------------------------------------------------------------------- ##

####################### #
# UI ####
####################### #

uiTop <- function(modPkg, pkgDes) {
  list(span(paste(modPkg,':'), id="rpkg"),
       span(pkgDes, id="pkgDes"),
       br())
}

uiBottom <- function(modAuthors=NULL, pkgName=NULL, pkgAuthors) {
  if (is.null(modAuthors)) {
    list(span(pkgName, id = "rpkg"), "references", br(),
         div(paste('Package Developers:', pkgAuthors), id="pkgDes"),
         a("CRAN", href = file.path("http://cran.r-project.org/web/packages", pkgName, "index.html"), target = "_blank"),
         " | ",
         a("documentation", href = file.path("https://cran.r-project.org/web/packages", pkgName, paste0(pkgName, ".pdf")), target = "_blank"))
  } else {
    if (is.null(pkgName)) {
      list(div(paste('Module Developers:', modAuthors), id="pkgDes"))
    } else {
      list(div(paste('Module Developers:', modAuthors), id="pkgDes"),
           span(pkgName, id = "rpkg"), "references", br(),
           div(paste('Package Developers:', pkgAuthors), id="pkgDes"),
           a("CRAN", href = file.path("http://cran.r-project.org/web/packages", pkgName, "index.html"), target = "_blank"),
           " | ",
           a("documentation", href = file.path("https://cran.r-project.org/web/packages", pkgName, paste0(pkgName, ".pdf")), target = "_blank")
      )
    }
  }
}

####################### #
# LOG WINDOW ####
####################### #

# initialize log window text
logInit <- function() {
  intro <- '***WELCOME TO WALLACE***'
  brk <- paste(rep('------', 14), collapse='')
  expl <- 'Please find messages for the user in this log window.'
  return(c(paste(intro, brk, expl, brk, sep='<br>')))
}

# add text to log
writeLog <- function(mp, ..., type = 'default') {
  if (type == "default") {
    pre <- "> "
  } else if (type == 'error') {
    pre <- '<font color="red"><b>! ERROR</b></font> : '
  } else if (type == 'warning') {
    pre <- '<font color="orange"><b>! WARNING</b></font> : '
  }
  
  args <- list(pre, ...)
  newEntries <- paste(args, collapse = ' ')
  isolate({mp$logs <- paste(mp$logs, newEntries, sep = '<br>')})
}

####################### #
# MISC ####
####################### #

# for naming files
nameAbbr <- function(spname) {
  namespl <- strsplit(tolower(spname), " ")
  genusAbbr <- substring(namespl[[1]][1], 1, 1)
  fullNameAbbr <- paste0(genusAbbr, "_", namespl[[1]][2])
  return(fullNameAbbr)
}

formatSpName <- function(spName) paste(strsplit(spName, split=' ')[[1]], collapse='_')

fileNameNoExt <- function(f) {
  sub(pattern = "(.*)\\..*$", replacement = "\\1", f)
}

####################### #
# MAPPING ####
####################### #

# return the map center given the bounds
mapCenter <- function(bounds) {
  map_center <- c(bounds$east - ((bounds$east - bounds$west) / 2), bounds$north - ((bounds$north - bounds$south) / 2))
  map_center <- round(map_center, digits=3)
  return(map_center)
}

# mapping controls
map_plotLocs <- function(map, locs, fillColor='red', fillOpacity=0.2) {
  if (is.null(locs)) return(map)
  map %>% addCircleMarkers(data = locs, lat = ~latitude, lng = ~longitude,
                           radius = 5, color = 'red', fill = TRUE,
                           fillColor = fillColor, fillOpacity = fillOpacity,
                           weight = 2, popup = ~pop)
}

# zoom to occ pts
zoom2Occs <- function(map, occs) {
  # map %>% clearShapes()
  lati <- occs[,3]
  longi <- occs[,2]
  z <- smartZoom(longi, lati)
  map %>% fitBounds(z[1], z[2], z[3], z[4])
  
  
  ## this section makes letter icons for occs based on basisOfRecord
  # makeOccIcons <- function(width = 10, height = 10, ...) {
  #   occIcons <- c('H', 'O', 'P', 'U', 'F', 'M', 'I', 'L', 'A', 'X')
  #   files <- character(9)
  #   # create a sequence of png images
  #   for (i in 1:9) {
  #     f <- tempfile(fileext = '.png')
  #     png(f, width = width, height = height, bg = 'transparent')
  #     par(mar = c(0, 0, 0, 0))
  #     plot.new()
  #     points(.5, .5, pch = occIcons[i], cex = min(width, height) / 8, col='red', ...)
  #     dev.off()
  #     files[i] <- f
  #   }
  #   files
  # }
  # occIcons <- makeOccIcons()
  # iconList <- list(HUMAN_OBSERVATION=1, OBSERVATION=2, PRESERVED_SPECIMEN=3,
  #                  UNKNOWN_EVIDENCE=4, FOSSIL_SPECIMEN=5, MACHINE_OBSERVATION=6,
  #                  LIVING_SPECIMEN=7, LITERATURE_OCCURRENCE=8, MATERIAL_SAMPLE=9)
  # values$origOccs$basisNum <- unlist(iconList[values$origOccs$basisOfRecord])
  # proxy %>% addMarkers(data = values$origOccs, lat = ~latitude, lng = ~longitude,
  #                      layerId = as.numeric(rownames(values$origOccs)),
  #                      icon = ~icons(occIcons[basisNum]))
}

# zooms appropriately for any extent
smartZoom <- function(longi, lati) {
  lg.diff <- abs(max(longi) - min(longi))
  lt.diff <- abs(max(lati) - min(lati))
  if (lg.diff > 1) lg.diff <- 1
  if (lt.diff > 1) lt.diff <- 1
  c(min(longi-lg.diff), min(lati-lt.diff), max(longi+lg.diff), max(lati+lt.diff))
}

popUpContent <- function(x) {
  lat <- round(as.numeric(x['latitude']), digits = 2)
  lon <- round(as.numeric(x['longitude']), digits = 2)
  as.character(tagList(
    tags$strong(paste("occID:", x['occID'])),
    tags$br(),
    tags$strong(paste("Latitude:", lat)),
    tags$br(),
    tags$strong(paste("Longitude:", lon)),
    tags$br(),
    tags$strong(paste("Year:", x['year'])),
    tags$br(),
    tags$strong(paste("Inst. Code:", x['institutionCode'])),
    tags$br(),
    tags$strong(paste("Basis of Record:", x['basisOfRecord'])),
    tags$br(),
    tags$strong(paste("Occurrence ID:", x['occurrenceID'])),
    tags$br(),
    tags$strong(paste("Country:", x['country'])),
    tags$br(),
    tags$strong(paste("State/Prov.:", x['stateProvince'])),
    tags$br(),
    tags$strong(paste("Locality:", x['locality'])),
    tags$br(),
    tags$strong(paste("Elevation:", x['elevation']))
  ))
}

####################### #
# COMP 3 ####
####################### #

remEnvsValsNA <- function(rvs) {
  withProgress(message = i18n$t("Checking for points with NA values..."), {
    occsVals <- raster::extract(rvs$envs, rvs$occs[c('longitude', 'latitude')])
    na.rowNums <- which(rowSums(is.na(occsVals)) > 1)
    
    if (length(na.rowNums) == length(occsVals)) {
      rvs %>% writeLog(type = 'error', i18n$t("No localities overlay with environmental predictors. All localities may be marine -- please redo with terrestrial occurrences."))
      return()
    }
    
    if (length(na.rowNums) > 0) {
      occs.notNA <- rvs$occs[-na.rowNums,]
      rvs %>% writeLog(type = 'warning', i18n$t("Removed records without environmental values with occIDs: "),
                        paste(rvs$occs[na.rowNums,]$occID, collapse=', '), ".")
      return(occs.notNA)
    }
    
    return(rvs$occs)
  })
}

####################### #
# COMP 4 ####
####################### #

# make a minimum convex polygon as SpatialPolygons object
mcp <- function(xy) {
  xy <- as.data.frame(sp::coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(as.matrix(xy.bord))), 1))))
}

####################### #
# COMP 5 ####
####################### #

comp5_map <- function(map, occs, occsGrp) {
  if (is.null(occsGrp)) return()
  # colors for partition symbology
  newColors <- gsub("FF$", "", rainbow(max(occsGrp)))  
  partsFill <- newColors[occsGrp]
  map %>%
    clearMarkers() %>%
    map_plotLocs(occs, fillColor = partsFill, fillOpacity = 1) %>%
    addLegend("bottomright", colors = newColors,
              title = "Partition Groups", labels = sort(unique(occsGrp)),
              opacity = 1, layerId = 'leg')
}

####################### #
# COMP 6 ####
####################### #
thresh <- function(modOccVals, type) {
  # remove all NA
  modOccVals <- na.omit(modOccVals)
  if (type == 'mtp') {
    # apply minimum training presence threshold
    x <- min(modOccVals)
  } else if (type == 'p10') {
    # Define 10% training presence threshold
    if (length(modOccVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
      n90 <- floor(length(modOccVals) * 0.9)
    } else {  # if greater than or equal to 10 occ values, round up
      n90 <- ceiling(length(modOccVals) * 0.9)
    }
    x <- rev(sort(modOccVals))[n90]  # apply 10% training presence threshold over all models
  }
  return(x)
}

####################### #
# COMP 7 ####
####################### #

predictMaxnet <- function(mod, envs, clamp, type) {
  requireNamespace("maxnet", quietly = TRUE)
  envs.n <- raster::nlayers(envs)
  envs.pts <- raster::getValues(envs) %>% as.data.frame()
  mxnet.p <- predict(mod, envs.pts, type = type,
                     clamp = clamp)
  envs.pts[as.numeric(row.names(mxnet.p)), "pred"] <- mxnet.p
  pred <- raster::rasterFromXYZ(cbind(raster::coordinates(envs),
                                      envs.pts$pred),
                                res = raster::res(envs),
                                crs = raster::crs(envs))
  return(pred)
}

# borrowed from the plot method for bioclim in dismo v.1.1-1
bc.plot <- function(x, a=1, b=2, p=0.9, ...) {
  
  d <- x@presence
  
  myquantile <- function(x, p) {
    p <- min(1, max(0, p))
    x <- sort(as.vector(stats::na.omit(x)))
    if (p == 0) return(x[1])
    if (p == 1) return(x[length(x)])
    i = (length(x)-1) * p + 1
    ti <- trunc(i)
    below = x[ti]
    above = x[ti+1]
    below + (above-below)*(i-ti)
  }
  
  p <- min(1,  max(0, p))
  if (p > 0.5) p <- 1 - p
  p <- p / 2
  prd <- dismo::predict(x, d, useC = FALSE)
  i <- prd > p & prd < (1-p)
  plot(d[,a], d[,b], xlab=colnames(d)[a], ylab=colnames(d)[b], cex=0)
  type=6
  x1 <- quantile(d[,a], probs=p, type=type)
  x2 <- quantile(d[,a], probs=1-p, type=type)
  y1 <- quantile(d[,b], probs=p, type=type)
  y2 <- quantile(d[,b], probs=1-p, type=type)
  polygon(rbind(c(x1,y1), c(x1,y2), c(x2,y2), c(x2,y1), c(x1,y1)), border='blue', lwd=2)
  points(d[i,a], d[i,b], xlab=colnames(x)[a], ylab=colnames(x)[b], col='green' )
  points(d[!i,a], d[!i,b], col='red', pch=3)
}

# make data.frame of lambdas vector from Maxent model object
lambdasDF <- function(mx, alg) {
  if (alg == "maxent.jar") {
    lambdas <- mx@lambdas[1:(length(mx@lambdas)-4)]
    data.frame(var = sapply(lambdas, FUN = function(x) strsplit(x, ',')[[1]][1]),
               coef = sapply(lambdas, FUN = function(x) as.numeric(strsplit(x, ',')[[1]][2])),
               row.names=1:length(lambdas))
  } else if (alg == "maxnet") {
    lambdas <- mx$betas
    data.frame(var = names(lambdas),
               coef = lambdas,
               row.names = 1:length(lambdas))
  }
}
## pulls out all non-zero, non-redundant (removes hinge/product/threshold) predictor names
mxNonzeroCoefs <- function(mx, alg) {
  if (alg == "maxent.jar") {
    x <- lambdasDF(mx, alg)
    #remove any rows that have a zero lambdas value (Second column)
    x <- x[(x[,2] != 0),]
    #remove any rows that have duplicate "var"s (hinges, quadratics, product)
    x <- unique(sub("\\^\\S*", "", x[,1]))
    x <- unique(sub("\\`", "", x))
    x <- unique(sub("\\'", "", x))
    x <- unique(sub("\\=\\S*", "", x))
    x <- unique(sub("\\(", "", x))
    x <- unique(unlist(strsplit(x, split = "\\*")))
    x <- sort(x)
  } else if (alg == "maxnet") {
    x <- lambdasDF(mx, alg)
    #remove any rows that have a zero lambdas value (Second column)
    x <- x[(x[,2] != 0),]
    #remove any rows that have duplicate "var"s (hinges, quadratics, product)
    x <- unique(sub("\\^\\S*", "", x[,1]))
    x <- unique(sub("\\I", "", x))
    x <- unique(sub("\\hinge", "", x))
    x <- unique(sub("\\categorical", "", x))
    x <- unique(sub("\\)\\:\\S*", "", x))
    x <- unique(sub("\\(", "", x))
    x <- unique(unlist(strsplit(x, split = "\\:")))
    x <- sort(x)
  }
}

respCurv <- function(mod, i) {  # copied mostly from dismo
  v <- rbind(mod@presence, mod@absence)
  v.nr <- nrow(v)
  vi <- v[, i]
  vi.r <- range(vi)
  expand <- 10
  xlm <- 25
  vi.rx <- seq(vi.r[1]-expand, vi.r[2]+expand, length.out=xlm)
  mm <- v[rep(1:v.nr, xlm), ]
  mm[, i] <- rep(vi.rx, v.nr)
  mm[, -i] <- rep(colMeans(mm[,-i]), each=nrow(mm))
  p <- predict(mod, mm)
  plot(cbind(vi.rx, p[1:xlm]), type='l', ylim=0:1, col = 'red', lwd = 2,
       ylab = 'predicted value', xlab = names(v)[i])
  pres.r <- range(mod@presence[, i])
  abline(v = pres.r[1], col='blue')  # vertical blue lines indicate min and max of presence vals
  abline(v = pres.r[2], col='blue')
  abs.r <- range(mod@absence[, i])
  abline(v = abs.r[1], col='green') # vertical green lines indicate min and max of background vals
  abline(v = abs.r[2], col='green')
  #graphics::text(x = vals, y = pred, labels = row.names(mod@presence), pos = 3, offset = 1)
}

getVals <- function(r, type='raw') {
  v <- raster::values(r)
  # remove NAs
  v <- v[!is.na(v)]
  if (type == 'logistic' | type == 'cloglog') v <- c(v, 0, 1)  # set to 0-1 scale
  return(v)
}

####################### #
# COMP 8 ####
####################### #

GCMlookup <- c(AC="ACCESS1-0", BC="BCC-CSM1-1", CC="CCSM4", CE="CESM1-CAM5-1-FV2",
               CN="CNRM-CM5", GF="GFDL-CM3", GD="GFDL-ESM2G", GS="GISS-E2-R",
               HD="HadGEM2-AO", HG="HadGEM2-CC", HE="HadGEM2-ES", IN="INMCM4",
               IP="IPSL-CM5A-LR", ME="MPI-ESM-P", MI="MIROC-ESM-CHEM", MR="MIROC-ESM",
               MC="MIROC5", MP="MPI-ESM-LR", MG="MRI-CGCM3", NO="NorESM1-M")

reverseLabels <- function(..., reverse_order = FALSE) {
  if (reverse_order) {
    function(type = "numeric", cuts) {
      cuts <- sort(cuts, decreasing = TRUE)
    }
  } else {
    labelFormat(...)
  }
}

comp8_map <- function(map, ras, polyXY, bgShpXY, rasVals, rasCols, 
                      legTitle, addID, clearID = NULL) {
  
  # if thresholded, plot with two colors
  if (grepl('thresh', names(ras))) {
    rasPal <- c('gray', 'blue')
    map %>% addLegend("bottomright", colors = c('gray', 'blue'),
                      title = "Thresholded Suitability", labels = c("predicted absence", "predicted presence"),
                      opacity = 1, layerId = 'leg')
  } else {
    rasPal <- colorNumeric(rasCols, rasVals, na.color='transparent')
    legPal <- colorNumeric(rev(rasCols), rasVals, na.color='transparent')
    map %>% addLegend("bottomright", pal = legPal, title = legTitle,
                      values = rasVals, layerId = 'leg',
                      labFormat = reverseLabels(2, reverse_order=TRUE))
  }
  map %>% 
    clearMarkers() %>% 
    clearShapes() %>%
    removeImage(clearID) %>%
    addRasterImage(ras, colors = rasPal, opacity = 0.9, 
                   group = 'c7', layerId = addID) %>%
    addPolygons(lng=polyXY[,1], lat=polyXY[,2], layerId="projExt", fill = FALSE,
                weight=4, color="green", group='c8')
  
  for (shp in bgShpXY()) {
    map %>%
      addPolygons(lng=shp[,1], lat=shp[,2], fill = FALSE,
                  weight=4, color="red", group='c8')  
  }
}

drawToolbarRefresh <- function(map) {
  map %>% leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) %>%
    leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                   rectangleOptions = FALSE, circleOptions = FALSE,
                                   markerOptions = FALSE, circleMarkerOptions = FALSE,
                                   editOptions = leaflet.extras::editToolbarOptions())
}

####################### #
# SESSION CODE ####
####################### #

makeCap <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
getSpName <- function() deparse(substitute(input$spName))
printVecAsis <- function(x) {
  if (is.character(x)) {
    if (length(x) == 1) {
      return(paste0("\'", x, "\'"))
    } else {
      return(paste0("c(", paste(sapply(x, function(a) paste0("\'", a, "\'")), collapse=", "), ")"))
    }
  } else {
    if (length(x) == 1) {
      return(x)
    } else {
      return(paste0("c(", paste(x, collapse=", "), ")"))
    }
  }
}

#####################
# Download utilities #
#####################

convert_list_cols <- function(x) {
  dplyr::mutate_if(.tbl = x,
                   .predicate = function(col) inherits(col, "list"),
                   .funs = function(col) {
                     vapply(col,
                            jsonlite::toJSON,
                            character(1L))
                   })
}

write_csv_robust <- function(x, ...) {
  write.csv(convert_list_cols(x), ...)
}

                                       

#####################
# Download world_clim #
#####################

# Download geographic data and return as R object
# Author: Robert J. Hijmans
# License GPL3
# Version 0.9
# October 2008


getDataEx <- function(name='GADM', download=TRUE, path='', ...) {

#	w <- getOption('rasterGetDataWarningGiven')
#	if (is.null(w)) {
		warning("getData will be removed in a future version of raster\n. Please use the geodata package instead")
#		options(rasterGetDataWarningGiven=TRUE)
#	}
	
	path <- .getDataPath(path)

	tout <- getOption("timeout")
	on.exit(options(timeout = tout))
	options(timeout = max(600, tout))
	
	if (name=='GADM') {
		.GADM(..., download=download, path=path)
	} else if (name=='SRTM') {
		.SRTM(..., download=download, path=path)
	} else if (name=='alt') {
		.raster(..., name=name, download=download, path=path)
	} else if (name=='worldclim') {
		.worldclimEx(..., download=download, path=path)
	} else if (name=='CMIP5') {
		.cmip5(..., download=download, path=path)
	} else if (name=='ISO3') {
		ccodes()[,c(2,1)]
	} else if (name=='countries') {
		.countries(download=download, path=path, ...)
	} else {
		stop(name, ' not recognized as a valid name.')
	}
}


.downloadEx <- function(aurl, filename) {
	fn <- paste(tempfile(), '.download', sep='')
	res <- utils::download.file(url=aurl, destfile=fn, quiet = FALSE, mode = "wb", cacheOK = TRUE)
	if (res == 0) {
		w <- getOption('warn')
		on.exit(options('warn' = w))
		options('warn'=-1) 
		if (! file.rename(fn, filename) ) { 
			# rename failed, perhaps because fn and filename refer to different devices
			file.copy(fn, filename)
			file.remove(fn)
		}
	} else {
		stop('could not download the file' )
	}
}

.ISO <- function() {
   ccodes()
}

ccodes <- function() {
	path <- system.file(package="raster")
	#d <- utils::read.csv(paste(path, "/external/countries.csv", sep=""), stringsAsFactors=FALSE, encoding="UTF-8")
	readRDS(file.path(path, "external/countries.rds"))
}


.getCountry <- function(country='') {
	country <- toupper(raster::trim(country[1]))

	cs <- ccodes()
	cs <- sapply(cs, toupper)
	cs <- data.frame(cs, stringsAsFactors=FALSE)
	nc <- nchar(country)

	if (nc == 3) {
		if (country %in% cs$ISO3) {
			return(country)
		} else {
			stop('unknown country')
		}
	} else if (nc == 2) {
		if (country %in% cs$ISO2) {
			i <- which(country==cs$ISO2)
			return( cs$ISO3[i] )
		} else {
			stop('unknown country')
		}
	} else if (country %in% cs[,1]) {
		i <- which(country==cs[,1])
		return( cs$ISO3[i] )
	} else if (country %in% cs[,4]) {
		i <- which(country==cs[,4])
		return( cs$ISO3[i] )
	} else if (country %in% cs[,5]) {
		i <- which(country==cs[,5])
		return( cs$ISO3[i] )
	} else {
		stop('provide a valid name name or 3 letter ISO country code; you can get a list with "ccodes()"')
	}
}

.dataloc <- function() {
	d <- getOption('rasterDataDir')
	if (is.null(d) ) {
		d <- getwd()
	} else {
		d <- trim(d)
		if (d=='') {
			d <- getwd()
		}
	}
	return(d)
}

.getDataPath <- function(path) {
	path <- raster::trim(path)
	if (path=="") {
		path <- .dataloc()
	} else {
		if (substr(path, nchar(path)-1, nchar(path)) == '//' ) {
			p <- substr(path, 1, nchar(path)-2)		
		} else if (substr(path, nchar(path), nchar(path)) == '/'  | substr(path, nchar(path), nchar(path)) == '\\') {
			p <- substr(path, 1, nchar(path)-1)
		} else {
			p <- path
		}
		if (!file.exists(p) & !file.exists(path)) {
			stop('path does not exist: ', path)
		}
	}
	if (substr(path, nchar(path), nchar(path)) != '/' & substr(path, nchar(path), nchar(path)) != '\\') {
		path <- paste(path, "/", sep="")
	}
	return(path)
}


.GADM <- function(country, level, download, path, version=3.6, type='sp') {
#	if (!file.exists(path)) {  dir.create(path, recursive=T)  }

	country <- .getCountry(country)
	if (missing(level)) {
		stop('provide a "level=" argument; levels can be 0, 1, or 2 for most countries, and higher for some')
	}

	if (version > 3) {
		if (type == 'sf') {
			filename <- file.path(path, paste0('gadm36_', country, '_', level, "_sf.rds"))
		} else {
			filename <- file.path(path, paste0('gadm36_', country, '_', level, "_sp.rds"))
		}
	} else {
		filename <- paste(path, 'GADM_', version, '_', country, '_', level, ".rds", sep="")	
	}
	
	if (!file.exists(filename)) {
		if (download) {
			baseurl <- paste0("https://geodata.ucdavis.edu/gadm", version)
			if (version == 2.8) {
				theurl <- paste(baseurl, '/rds/', country, '_adm', level, ".rds", sep="")			
			} else {
				if (type == 'sf') {
					theurl <- paste(baseurl, '/Rsf/gadm36_', country, '_', level, "_sf.rds", sep="")			
				} else {
					theurl <- paste(baseurl, '/Rsp/gadm36_', country, '_', level, "_sp.rds", sep="")			
				}
			}
			.downloadEx(theurl, filename)
			if (!file.exists(filename))	{ 
				message("\nCould not download file -- perhaps it does not exist") 
			}
		} else {
			message("File not available locally. Use 'download = TRUE'")
		}
	}	
	if (file.exists(filename)) {
		x <- readRDS(filename)
		# avoid pesky warnings
		if (type != 'sf') {
			crs(x) <- "+proj=longlat +datum=WGS84"
		}
		return(x)
	} else {
		return(NULL)
	}
}




.countries <- function(download, path, type='sp', ...) {

	if (type == 'sf') {
		f <- "countries_gadm36_sf.rds"
	} else {
		f <- "countries_gadm36_sp.rds"	
	}
	filename <- file.path(path, f)
	
	if (!file.exists(filename)) {
		if (download) {
			theurl <- paste0("https://geodata.ucdavis.edu/gadm3.6/", f)
			.downloadEx(theurl, filename)
			if (!file.exists(filename)) {
				message("\nCould not download file -- perhaps it does not exist") 
			}
		} else {
			message("File not available locally. Use 'download = TRUE'")
		}
	}	
	if (file.exists(filename)) {
		#thisenvir = new.env()
		#data <- get(load(filename, thisenvir), thisenvir)
		data <- readRDS(filename)
		crs(data) <- "+proj=longlat +datum=WGS84"
		return(data)
	} 
}


.cmip5 <- function(var, model, rcp, year, res, lon, lat, path, download=TRUE) {
	if (!res %in% c(0.5, 2.5, 5, 10)) {
		stop('resolution should be one of: 2.5, 5, 10')
	}
	if (res==2.5) { 
		res <- '2_5m' 
    } else if (res == 0.5) {
        res <- "30s"
    } else {
		res <- paste(res, 'm', sep='')
	}
	
	var <- tolower(var[1])
	vars <- c('tmin', 'tmax', 'prec', 'bio')
	stopifnot(var %in% vars)
	var <- c('tn', 'tx', 'pr', 'bi')[match(var, vars)]
	
	model <- toupper(model)
	models <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
	stopifnot(model %in% models)
	
	rcps <- c(26, 45, 60, 85)
	stopifnot(rcp %in% rcps)
	stopifnot(year %in% c(50, 70))
	
	#m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
	
	m <- matrix(c(0,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,0,1,1,1,0,1,0,1,1,1,1,0,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)

	i <- m[which(model==models), which(rcp==rcps)]
	if (!i) {
		warning('this combination of rcp and model is not available')
		return(invisible(NULL))
	}
	
	path <- paste(path, '/cmip5/', res, '/', sep='')
	dir.create(path, recursive=TRUE, showWarnings=FALSE)

	zip <- tolower(paste(model, rcp, var, year, '.zip', sep=''))
	theurl <- paste('https://geodata.ucdavis.edu/climate/cmip5/', res, '/', zip, sep='')

	zipfile <- paste(path, zip, sep='')
	if (var == 'bi') {
		n <- 19
	} else {
		n <- 12
	}
	tifs <- paste(raster::extension(zip, ''), 1:n, '.tif', sep='')
	files <- paste(path, tifs, sep='')
	fc <- sum(file.exists(files))
	if (fc < n) {
		if (!file.exists(zipfile)) {
			if (download) {
				.downloadEx(theurl, zipfile)
				if (!file.exists(zipfile))	{ 
					message("\n Could not download file -- perhaps it does not exist") 
				}
			} else {
				message("File not available locally. Use 'download = TRUE'")
			}
		}	
		utils::unzip(zipfile, exdir=dirname(zipfile))
	}
	raster::stack(paste(path, tifs, sep=''))
}

#.cmip5(var='prec', model='BC', rcp=26, year=50, res=10, path=getwd())


.worldclimEx <- function(var, res, lon, lat, path, download=TRUE) {
	if (!res %in% c(0.5, 2.5, 5, 10)) {
		stop('resolution should be one of: 0.5, 2.5, 5, 10')
	}
	if (res==2.5) { res <- '2-5' }
	stopifnot(var %in% c('tmean', 'tmin', 'tmax', 'prec', 'bio', 'alt'))
	path <- paste(path, 'wc', res, '/', sep='')
	dir.create(path, showWarnings=FALSE)

	if (res==0.5) {
		lon <- min(180, max(-180, lon))
		lat <- min(90, max(-60, lat))
		rs <- raster::raster(nrows=5, ncols=12, xmn=-180, xmx=180, ymn=-60, ymx=90 )
		row <- raster::rowFromY(rs, lat) - 1
		col <- raster::colFromX(rs, lon) - 1
		rc <- paste(row, col, sep='') 
		zip <- paste(var, '_', rc, '.zip', sep='')
		zipfile <- paste(path, zip, sep='')
		if (var  == 'alt') {
			bilfiles <- paste(var, '_', rc, '.bil', sep='')
			hdrfiles <- paste(var, '_', rc, '.hdr', sep='')			
		} else if (var  != 'bio') {
			bilfiles <- paste(var, 1:12, '_', rc, '.bil', sep='')
			hdrfiles <- paste(var, 1:12, '_', rc, '.hdr', sep='')
		} else {
			bilfiles <- paste(var, 1:19, '_', rc, '.bil', sep='')
			hdrfiles <- paste(var, 1:19, '_', rc, '.hdr', sep='')		
		}
		theurl <- paste('https://geodata.ucdavis.edu/climate/worldclim/1_4/tiles/cur/', zip, sep='')
	} else {
		zip <- paste(var, '_', res, 'm_bil.zip', sep='')
		zipfile <- paste(path, zip, sep='')
		if (var  == 'alt') {
			bilfiles <- paste(var, '.bil', sep='')
			hdrfiles <- paste(var, '.hdr', sep='')			
		} else if (var  != 'bio') {
			bilfiles <- paste(var, 1:12, '.bil', sep='')
			hdrfiles <- paste(var, 1:12, '.hdr', sep='')
		} else {
			bilfiles <- paste(var, 1:19, '.bil', sep='')
			hdrfiles <- paste(var, 1:19, '.hdr', sep='')	
		}
		theurl <- paste('https://geodata.ucdavis.edu/climate/worldclim/1_4/grid/cur/', zip, sep='')
	}
	files <- c(paste(path, bilfiles, sep=''), paste(path, hdrfiles, sep=''))
	fc <- sum(file.exists(files))
	
	
	if ( fc < length(files) ) {
		if (!file.exists(zipfile)) {
			if (download) {
				.downloadEx(theurl, zipfile)
				if (!file.exists(zipfile))	{ 
					message("\n Could not download file -- perhaps it does not exist") 
				}
			} else {
				message("File not available locally. Use 'download = TRUE'")
			}
		}	
		utils::unzip(zipfile, exdir=dirname(zipfile))
		for (h in paste(path, hdrfiles, sep='')) {
			x <- readLines(h)
			x <- c(x[1:14], 'PIXELTYPE     SIGNEDINT', x[15:length(x)])
			writeLines(x, h)
		}
	}
	if (var  == 'alt') {
		st <- raster::raster(paste(path, bilfiles, sep=''))
	} else {
		st <- raster::stack(paste(path, bilfiles, sep=''))
	}
	raster::projection(st) <- "+proj=longlat +datum=WGS84"
	return(st)
}



.raster <- function(country, name, mask=TRUE, path, download, keepzip=FALSE, ...) {

	country <- .getCountry(country)
	path <- .getDataPath(path)
	if (mask) {
		mskname <- '_msk_'
		mskpath <- 'msk_'
	} else {
		mskname<-'_'
		mskpath <- ''		
	}
	filename <- paste(path, country, mskname, name, ".grd", sep="")
	if (!file.exists(filename)) {
		zipfilename <- filename
		raster::extension(zipfilename) <- '.zip'
		if (!file.exists(zipfilename)) {
			if (download) {
				theurl <- paste("https://geodata.ucdavis.edu/diva//", mskpath, name, "/", country, mskname, name, ".zip", sep="")
				.downloadEx(theurl, zipfilename)
				if (!file.exists(zipfilename))	{ 
					message("\nCould not download file -- perhaps it does not exist") 
				}
			} else {
				message("File not available locally. Use 'download = TRUE'")
			}
		}
		ff <- utils::unzip(zipfilename, exdir=dirname(zipfilename))
		if (!keepzip) {
			file.remove(zipfilename)
		}
	}	
	if (file.exists(filename)) { 
		rs <- raster::raster(filename)
	} else {
		#patrn <- paste(country, '.', mskname, name, ".grd", sep="")
		#f <- list.files(path, pattern=patrn)
		f <- ff[substr(ff, nchar(ff)-3, nchar(ff)) == '.grd']
		if (length(f)==0) {
			warning('something went wrong')
			return(NULL)
		} else if (length(f)==1) {
			rs <- raster::raster(f)
		} else {
			rs <- sapply(f, raster)
			message('returning a list of RasterLayer objects')
			return(rs)
		}
	}
	raster::projection(rs) <- "+proj=longlat +datum=WGS84"
	return(rs)	
}



.SRTM <- function(lon, lat, download, path) {
	stopifnot(lon >= -180 & lon <= 180)
	stopifnot(lat >= -60 & lat <= 60)
	
	rs <- raster::raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
	rowTile <- raster::rowFromY(rs, lat)
	colTile <- raster::colFromX(rs, lon)
	if (rowTile < 10) { rowTile <- paste('0', rowTile, sep='') }
	if (colTile < 10) { colTile <- paste('0', colTile, sep='') }

	baseurl <- "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/"
	
	f <- paste0("srtm_", colTile, "_", rowTile, ".zip")

	zipfilename <- file.path(path, f)
	tiffilename <- file.path(path, gsub(".zip$", ".tif", f))
	
	if (!file.exists(tiffilename)) {
		if (!file.exists(zipfilename)) {
			if (download) { 
				theurl <- paste0(baseurl, f)
				test <- try (.downloadEx(theurl, zipfilename) , silent=TRUE)
				if (inherits(test, "try-error")) {
					stop("cannot download the file")
				}
			} else {message("file not available locally, use download=TRUE") }	
		} 
		if (file.exists(zipfilename)) { 
			utils::unzip(zipfilename, exdir=dirname(zipfilename))
			file.remove(zipfilename)
		}	
	}
	if (file.exists(tiffilename)) { 
		rs <- raster::raster(tiffilename)
		raster::projection(rs) <- "+proj=longlat +datum=WGS84"
		return(rs)
	} else {
		stop('file not found')
	}
}

#.SRTM(lon=5.5, lat=44.5, TRUE, ".")
