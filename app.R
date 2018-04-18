#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# TODO: Add :
# DONE: 1. Control Point loading
# DONE: 2. Add CP managing
# DONE: 3. linear models evaluation
# DONE: 4. Linear models results presentation
# DONE: 5. Multiple (more than 2) maps loading 
# INPR: 6. Maps Management
# TODO: 7. provide different Prediction types 
# TODO: 7.1. classification tools
# TODO: 7.2. Neural networks tools
# TODO: 7.3. Linear Modles types selection
# TODO: 7.4. Gaussian Mixture model clastering
# TODO: 8. Add cross-validation analysis (if not included in LM)
# TODO: 9. Provide batch maps loading (by path and extension)
# TODO: FIX Well labeling order issue



library(shiny)
library(shinyjs)
library(DT)

library(readr)
library(raster)

#library(spatial)
library(sp)
library(RANN)
library(ClusterR)
library(hexbin)
#library(markdown)

#default data definition
map0 <- sp::read.asciigrid("../Data/Maps/default.asc")
fn0 <- "default.asc"
names(map0@data) <- fn0
rstr0 <- raster(map0)
def_map <- list("fn" = fn0,
                "map" = map0,
                "mat" = as.vector(rstr0),
                "rstr" = rstr0)
wells0 = NULL
maps0 = list(NULL,NULL) #list(def_map,def_map)
map_zoom = NULL

flist <- list(mean,median,sd)
names(flist) <- c("Среднее","Медиана","Ст.Откл.")

map_hei = "900px"
map_wid = "800px"
hist_hei = "200px"
hist_wid = "200px"
butt_wid = "200px"

myReactives <- reactiveValues(wells = wells0, zoom = map_zoom, fit = NULL, maps = NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Кроссплоты карт"),
  # test = enc2utf8(c("привет","пока"))
  
  
  #UI: Sidebar with a slider input for number of bins ####
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel( "Гексаплот",
          plotOutput(
            "xPlot_hex",
            click = "plot_click",
            dblclick = "plot_dblclick",
            hover = "plot_hover",
            brush = brushOpts( id = "plot_brush", clip = FALSE, resetOnNew = TRUE)
          ),
          sliderInput(
            "cells",
            "размер бина кроссплота:",
            min = 1,
            max = 50,
            value = 30
          )
          ),
          tabPanel( "Кроссплот",
                  plotOutput(
                    "xPlot_simple",
                    click = "plot_click",
                    dblclick = "plot_dblclick",
                    hover = "plot_hover",
                    brush = brushOpts( id = "plot_brush", clip = FALSE, resetOnNew = TRUE)
                  ),
            sliderInput(
              "transp",
              "Прозрачность:",
              min = 0.1,
              max = 2,
              value = 1
            )
          ),
        #UI: Wells inputs and display ####
        tabPanel(
          "Скважины",
          fileInput(
            "wellsfile1",
            "Открыть скважины:",
            accept = c("text/plain",
                       "text/esri-asciigrid,text/plain",
                       "*.asc",
                       buttonLabel = "Открыть...")
          ),
          fileInput(
            "cpfile1",
            "Открыть КТ:",
            accept = c("text/plain",
                       "text/esri-asciigrid,text/plain",
                       "*.asc",
                       buttonLabel = "Открыть...")
          ),
          dataTableOutput('table_wells')
        )  ,
        #UI: Maps inputs  ####
        tabPanel(
          "Карты",

          sliderInput(
            "bins",
            "Бины гистограммы:",
            min = 1,
            max = 100,
            value = 30
          ),
          sliderInput(
            "rstr_fact",
            "Разрешение грида:",
            min = 0.1,
            max = 1,
            value = 0.5
          ),
          fileInput(
            "Mapsfile",
            "Открыть/Заменить карту:",
            accept = c("text/plain",
                       "text/esri-asciigrid,text/plain",
                       "*.asc",
                       buttonLabel = "Открыть...")
          ),
          dataTableOutput('table_maps')
        )  ,
        #UI: models ####
        tabPanel(
          "Модели",
          tabsetPanel(
            #UI: model MLR ####
            tabPanel(
              "MLR",
              plotOutput(
                "fitPlot",
                height = "500px"
              ),
              verbatimTextOutput("fitText"),
              plotOutput(
                "fitxPlot",
                height = "500px"
              ),
              verbatimTextOutput("fitXText")
            ),
            #UI: model GLM ####
            tabPanel(
              "GLM",
              plotOutput(
                "glmPlot",
                height = "500px"
              ),
              verbatimTextOutput("glmText"),
              plotOutput(
                "glmXPlot",
                height = "500px"
              ),
              verbatimTextOutput("glmXText")
              ),
            #UI: model KM ####
            tabPanel(
              "K-Means",
              plotOutput(
                "kmPlot",
                height = "500px"
              ),
              sliderInput(
                "kmClasses",
                "Число Классов:",
                min = 1,
                max = 10,
                value = 3
              ),
              verbatimTextOutput("kmText"),
              plotOutput(
                "kmXPlot",
                height = "500px"
              ),
              verbatimTextOutput("kmXText")            
              ),
            #UI: model GMM ####
            tabPanel(
              "GMM K-Means",
              plotOutput(
                "gmmPlot",
                height = "500px"
              ),
              sliderInput(
                "gmmClasses",
                "Число Классов:",
                min = 1,
                max = 10,
                value = 3
              ),
              verbatimTextOutput("gmmText"),
              plotOutput(
                "gmmXPlot",
                height = "500px"
              ),
              verbatimTextOutput("gmmXText")            
            )
          )
        )
      )
      
      # Show a plot of the generated distribution
      ,width = 4, fluid = FALSE),
    #), width = 3 ), #
    mainPanel(
      
      tabsetPanel(
        # UI: Tabs with Maps and related ####
        #tabPanel(
        #  "Скважины",
        #   dataTableOutput('table_wells')
        #),
        tabPanel(
          #UI: Tab1 ####
          uiOutput("tab1"),
          flowLayout(
          flowLayout(
            verticalLayout(
              flowLayout(
              selectInput("funcSelect1", "Метод осреднения"
                          ,choices = names(flist), width = butt_wid
                          )
              ),
              flowLayout(
                actionButton("unzoom1"   , "Сброс масштаба  ", width = butt_wid),
                actionButton("trans1"    , "Транспонировать ", width = butt_wid)
              )
            ),
            plotOutput("histPlot1",  
                       height = hist_hei,
                       width = hist_wid
            )
            , cellWidths = c("80%","20%")
          ),
          #UI: Map1 ####
          plotOutput(
            "mapPlot1",
            click = "plot_click",
            dblclick = "plot_dblclick",
            hover = "plot_hover",
            brush = "plot_brush",
            height = map_hei,
            width = map_wid
          )
          )
        ),
        tabPanel(
          #UI: Tab2 ####
          uiOutput("tab2"),
          flowLayout(
            flowLayout(
            verticalLayout(
              flowLayout(
                selectInput("funcSelect2", "Метод осреднения"
                            ,choices = names(flist), width = butt_wid
                )
              ),
              flowLayout(
                actionButton("unzoom2"   , "Сброс масштаба  ", width = butt_wid),
                actionButton("trans2"    , "Транспонировать ", width = butt_wid)
              )
            ),
            plotOutput("histPlot2",  
                       height = hist_hei,
                       width = hist_wid
            )
            , cellWidths = c("80%","20%")
          ),
          #UI: Map2 ####
          plotOutput(
            "mapPlot2",
            click = "plot_click",
            dblclick = "plot_dblclick",
            hover = "plot_hover",
            brush = "plot_brush",
            height = map_hei,
            width = map_wid
          )
        )
        )
      ),
      #UI: Status panel ####
      verbatimTextOutput("info")
    )
    
  )
)

## transpose the maps' matrix 
transposeMap <- function(map_obj = NULL) {
  if(is.null(map_obj)) return(NULL)
  c=map_obj$rstr@ncols 
  r=map_obj$rstr@nrows
  map_obj$rstr@data@values = matrix(map_obj$rstr@data@values, ncol = c, nrow = r, byrow = TRUE)
#  map_obj$map@data = data.frame(as.vector(map_m))
  
  map_obj$rstr@ncols=r
  map_obj$rstr@nrows=c
  
  #map_obj$rstr = raster(map_obj$map)
  map_obj$mat = as.vector(map_obj$rstr)
  return(map_obj)
}

## upscale/dense the map to defined fraction with defined aggregation method
upscaleMap <- function(map_obj = NULL, fact = 1.0, func = mean) {
  if(is.null(map_obj)) return(NULL)

  rstr = raster(map_obj$map)
  if (fact > 1.0) 
    rstr = disaggregate(rstr,fact = fact, method = 'bilinear')
  else if (fact < 1.0) 
    rstr = aggregate(rstr,fact = ceiling(1.0/fact), expand = TRUE, fun = func)
  map_obj$rstr = rstr
  map_obj$mat = as.vector(rstr)
  
  return(map_obj)
}

## load map file in ESRI ascii grid format, with optional transposing of the matrix ##
loadMapFile <- function (file_obj = NULL, transpose = FALSE) {
  if (is.null(file_obj)) {
    map = try( expr = sp::read.asciigrid("../Data/Maps/default.asc") , TRUE)
    if(class(map)=="try-error"){
      return(NULL)
    }
    fn = "default.asc"
  } else {
    map = try( expr = sp::read.asciigrid(file_obj$datapath), TRUE)
    if(class(map)=="try-error"){
      return(NULL)
    }
    fn = file_obj$name
  }
  
  names(map@data) = fn
  rstr = raster(map)
  map_m = as.vector(rstr)
  names(map_m) = fn
  map_obj = list(
    "fn" = fn,
    "map" = map,
    "mat" = map_m,
    "rstr" = rstr
  )
  #transpose the data.frame
  if(transpose) {
    map_obj = transposeMap(map_obj)
  }
  return(map_obj)
}

## load wells coordinates for display and store the CP data ##
loadWells <- function(file_obj = NULL) {
  wells = try ( expr = read_delim(
    file_obj$datapath,"\t ",
    col_types = cols(COORDINATE_SYS_PRJ = col_skip(), 
                     Customized_SYMBOL = col_skip(), DATUM_ELEVATION = col_skip(), 
                     DB_Access = col_skip(), ELEV_MEAS_REF = col_double(), 
                     EPOS_CRS = col_skip(), LATITUDE = col_skip(), 
                     LONGITUDE = col_skip(), WELL = col_character(), 
                     X12 = col_skip(), X_LOCATION = col_double(), 
                     Y_LOCATION = col_double()),
    na = "empty", trim_ws = TRUE
  ), TRUE)
  ers <- try (expr = problems(wells),TRUE)
  
  if( class(wells)=="try-error" || 
      dim(ers)[1]>0 || 
      !(c("WELL","X_LOCATION","Y_LOCATION") %in% names(wells) ) 
      ) 
    return(NULL)
  
  wells_ = list( WELL = wells$WELL)
  wells_$X_LOCATION = wells$X_LOCATION
  wells_$Y_LOCATION = wells$Y_LOCATION
  wells_$Values = c(rep(NA,length(wells$WELL)))
  wells_ = as.data.frame(wells_)
  coordinates(wells_) = ~X_LOCATION+Y_LOCATION
  return(wells_)
}

extractMap2Well <- function (wells = NULL, rstr = NULL, name = "Map") {
  if(is.null(wells) || is.null(rstr)) return(NULL)
  
  r_atwells = extract(rstr,data.frame(wells$X_LOCATION,wells$Y_LOCATION))
  wls = cbind(X1 = wells$X_LOCATION,X2 =wells$Y_LOCATION, X3 = as.vector(r_atwells))
  
  wls = addWellCP(wells,as.data.frame(wls),TRUE, name)
  return(wls)
}

addWellCP <- function (wells = NULL, cpdata = NULL, replace = TRUE, cpname = "Value") {
  if(is.null(wells) || is.null(cpdata)) return(NULL)
  
  wls = as.data.frame(wells)
  # this applies to Data.frames
  # 
  if(!replace) cpname=cpFile$name
  
  
  closest <- nn2( cbind(wls$X_LOCATION,wls$Y_LOCATION), cbind(cpdata$X1,cpdata$X2), 
                    k=1,searchtype = "radius", radius = 10)

  if(length(closest[!is.na(closest$nn.idx)])<1) return(NULL)
  

  wls[[cpname]] = c(1:length(wells[,1]))
  wls[[cpname]] = NA
  wls[closest$nn.idx,cpname] = cpdata$X3
  coordinates(wls) = ~X_LOCATION+Y_LOCATION
  return(wls)
  
}


# load CP data into the current wells coordinates or create wells from coordiantes in the file
loadCP <- function (wells = NULL, cpFile , tolerance = 10, replace = TRUE, cpname = "Value") {
  cpdata =  try ( expr = read_table2(cpFile$datapath,col_names = F), TRUE)
  ers <- try (expr = problems(cpdata),TRUE)
  if( class(wells)=="try-error" || 
      dim(ers)[1]>0   ) 
    return(NULL)
  if( is.null(wells)) {
    return(NULL)
    wls =  list( WELL = c(1:length(cpdata$X1)) )
    wls$X_LOCATION = cpdata$X1
    wls$Y_LOCATION = cpdata$X2
    wls[[cpname]] = cpdata$X3
    wls = as.data.frame(wls)
    coordinates(wls) = ~X_LOCATION+Y_LOCATION
    
    return(wls)
  }

  return (addWellCP(wells,cpdata,TRUE,cpname))
}

# Draw single map
drawMap <- function (map = def_map$map, fact = 1.0, zoom = NULL) {
  meanx = (map@bbox[1, 1] + map@bbox[1, 2]) / 2
  meany = (map@bbox[2, 1] + map@bbox[2, 2]) / 2
  
  dx = map@bbox[1, 2] - map@bbox[1, 1]
  dy = map@bbox[2, 2] - map@bbox[2, 1]
  
  offx = (meanx - map@bbox[1, 1])
  offy = (meanx - map@bbox[2, 1])
  
  scale_unit = max(abs(dx), abs(dy)) / 5
  scale_num =
    (scale_unit - scale_unit %% (10 ^ floor(log10(scale_unit))))
  
  loc_arrow = c(meanx - abs(dx - dx / 3), meany - abs(dy - dx / 2))
  loc_scale_txt_min = c(meanx - abs(dx - dx / 3), meany - abs(dy - dx /
                                                                2.1))
  loc_scale_txt_max =
    c(meanx - abs(dx - dx / 3) + scale_num, meany - abs(dy - dx / 2.1))
  loc_scale = c(meanx - abs(dx - dx / 3), meany - abs(dy - dx / 2.3))
  
  scale = list(
    "SpatialPolygonsRescale",
    layout.scale.bar(),
    offset = loc_scale,
    scale = scale_unit,
    fill = c("transparent", "black")
  )
  text1 = list("sp.text", loc_scale_txt_min, 0)
  text2 = list("sp.text", loc_scale_txt_max, paste(scale_num, " m"))
  arrow = list(
    "SpatialPolygonsRescale",
    layout.north.arrow(),
    offset = loc_arrow,
    scale = scale_unit / 2
  )
  #  spplot(map
  #    map,
  #    sp.layout = list(arrow, scale, text1, text2, arrow),
  #    scales = list(draw = T),
  #    xlim = bbexpand(bbox(map)[1,], 0.3),
  #    ylim = bbexpand(bbox(map)[2,], 0.1),
  #    colorkey = list(space = "right", height = 0.4)
  #  )
  #par(new = TRUE)
  #if (fact > 1)
  #  rstr = disaggregate(raster(map),fact = fact, method = 'bilinear')
  #else if (fact < 1)
  #  rstr = aggregate(raster(map),fact = 1/fact, fun = mean)
  #else rstr = raster(map)
  
  if(!is.null(zoom)) {
    plot(map, xlim = zoom[1,], ylim = zoom[2,])
  } 
  else {
    zoom <- NULL
    par(new = TRUE)
    plot(map)
  }
}

selectMap <- function (maps = NULL, sr = NULL, idx = -1) {
  if(is.null(maps) || idx<1 || idx > length(maps)) return (NULL)
  
  #if ( is.null(sr) || length(sr) < 1 ) map_obj = maps[[1]]
  #else map_obj = maps[[sr[1]]]
  
  if ( is.null(sr) || length(sr) < 1 ) map_idx = idx
  else map_idx = sr[idx]
  
  
  return(map_idx)
}

drawRstr <- function (map = def_map$rstr, zoom = NULL) {

  if(!is.null(zoom)) {
    plot(map, xlim = zoom[1,], ylim = zoom[2,])
  } 
  else {
    zoom <- NULL
    par(new = TRUE)
    plot(map)
  }
}

drawWells <- function(wells = NULL, rstr = NULL, sr = NULL) {
  if(!is.null(wells)) {
    #par(new = TRUE)
    plot(wells, add = TRUE, pch = 21)
    text(wells, labels = wells$WELL, cex = .7, pos = 1)

    
    drawParTxt <- function(wls = NULL, id = 1, pos = 1, sr = NULL) {
      if(length(wls@data[1,]) < id) return()

      labs = prettyNum(wls@data[,id][!is.na(wls@data[,id])], 
                       digits = 2, drop0trailing = TRUE)

      clrs = rep("black", times = length(wls@data$WELL))
      if(!is.null(sr)) {
        clrs[sr] = "red"
      }
      
      if(length(labs)>0) text(wls[!is.na(wls@data[,id]),], labels = labs, cex = .7, pos = pos, col = clrs[!is.na(wls@data[,id])])
    }
    
    drawParTxt(wells,2,2, sr) # Map1
    drawParTxt(wells,3,4, sr) # Map2
    drawParTxt(wells,4,3, sr) # Value
    
    
  }
  
}


drawHist <- function (map_m, nbins) {
  x <- map_m[!is.na(map_m)]
  bins <- seq(min(x), max(x), length.out = nbins + 1)
  # draw the histogram with the specified number of bins
  hist(
    x,
    breaks = bins,
    col = 'darkgray',
    border = 'darkgray',
    xlab = paste(length(x),"samples"),
    main = paste("Гистограмма ",names(map_m))
  )
}

drawHex <- function (xy = NULL, cells = 30) {
  if(is.null(xy)) 
    return()
  xccf <- ccf(xy[[2]], xy[[1]], lag.max = 0)
  par(cex = 0.2,
      cex.lab = 0.2,
      cex.axis = 0.2,
      cex.main = 0.2,
      cex.sub = 0.2,
      mai = c(0,0,0,0),
      new = TRUE)
  hexbinplot(x = xy[[2]]~xy[[1]],data = xy,
             xlab = names(xy)[1],
             ylab = names(xy)[2],
             #aspect = "iso",
             main = sprintf("Кроссплот CC=%5.2f %s=%5.2f", xccf$acf, parse(text = 'R^2'), as.numeric(xccf$acf) ^ 2),
             xbins = cells
             #colramp = rainbow
             
  )
  #text(c(1,1),labels = basename(myReactives$map1$fn))
}

buildMLR <- function(wells = NULL, rows = NULL){
  if(is.null(wells) || length(wells@data[1,])<4) return(NULL)
  
  data = wells@data
  n1 = length(data$Values[!is.na(data$Values)])
  n2 = length(data$Map1[!is.na(data$Map1)])
  n3 = length(data$Map2[!is.na(data$Map2)])
  if(n1<1 || n2<1 || n3<1) return(NULL)
  row.names(data) = data$WELL
  if(is.null(rows)) fit = lm(Values ~ Map1 + Map2, data)
  else {
    sel = rep( TRUE, times = length(data$WELL))
    sel[rows] = FALSE
    fit = lm(Values ~ Map1 + Map2, data[sel,])
    #names(fit$residuals) = data$WELL[sel]
  }
  return(fit)
}

buildMLRmul <- function(wells = NULL, rows = NULL){
  if(is.null(wells) || length(wells@data[1,])<4) return(NULL)
  
  data = wells@data
  
  for (par in  2:length(data) ) {
    if (length(data$Map1[!is.na(data[[par]])]) <1) return(NULL)
  }
  
  row.names(data) = data$WELL
  parnames = colnames(data)[2:length(data)]
  frml = as.formula(paste("Values~", paste(parnames[-1],collapse = "+")))
  if(is.null(rows)) fit = lm(formula = frml, data)
  else {
    sel = rep( TRUE, times = length(data$WELL))
    sel[rows] = FALSE
    fit = lm(formula = frml, data[sel,])
    #names(fit$residuals) = data$WELL[sel]
  }
  return(fit)
}

buildGLM <- function(wells = NULL, rows = NULL, lmfunc = glm, family = gaussian){
  if(is.null(wells) || length(wells@data[1,])<4) return(NULL)
  
  data = wells@data
  for (par in  2:length(data) ) {
    if (length(data$Map1[!is.na(data[[par]])]) <1) return(NULL)
  }
  row.names(data) = data$WELL
  parnames = colnames(data)[2:length(data)]
  frml = as.formula(paste("Values~", paste(parnames[-1],collapse = "+")))
  if(is.null(rows)) fit = lmfunc(formula = frml, data, family = family)
  else {
    sel = rep( TRUE, times = length(data$WELL))
    sel[rows] = FALSE
    fit = lmfunc(formula = frml, data[sel,], family = family)
    #names(fit$residuals) = data$WELL[sel]
  }
  return(fit)
}


drawModelQC <- function(fit = NULL){
  if(is.null(fit)) return()

  par(mfrow = c(2,2))
  plot(fit)
  return(fit)
}

drawModelXplot <- function(data = NULL, lmfit = NULL, srows = NULL) {
  sel = c( 1:length(data$WELL))
  sel[srows] = NA
  sel = sel[!is.na(data$Values)]
  sel = sel[!is.na(sel)]
  predicted = predict.lm(lmfit)
  measured = data$Values[sel]
  abl = lm(predicted~measured)
  #sel[input$table_wells_rows_selected] = FALSE
  #sel <- sel[!is.na(myReactives$wells@data$Values)]
  
  xccf = ccf(measured, predicted, lag.max = 0, plot = F)
  tit = sprintf("Кроссплот CC=%5.2f Rsq=%5.2f", xccf$acf, as.numeric(xccf$acf) ^ 2)
  
  plot(measured, predicted, main = tit, 
       xlim = bbexpand(c(min(measured),max(measured)),0.1),
       ylim = bbexpand(c(min(predicted),max(predicted)),0.1))
  abline(abl)
  #text(mean(measured),min(predicted) + (max(predicted)-min(predicted))*1.1,
  #     paste(abl$call$formula[2]," = ", 
  #           abl$call$formula[3]," * ",prettyNum(abl$coefficients[2]),"+",
  #           prettyNum(abl$coefficients[1])))
  text(measured,predicted, labels = data$WELL[sel], pos = 1)
  

}

drawKMeans <- function(maps = NULL, nclass = 3) {
  if(is.null(maps)) return(NULL)

  data = getLiveMapsData(maps)
  #browser()
  kmns = kmeans(center_scale(data[,2:length(data[1,])]),nclass)
  clr=kmns$cluster
  plot(as.data.frame(data[,2:length(data[1,])]),col = rainbow(nclass)[clr], pch = 16)
  return(kmns)
}

getLiveMapsData <- function (maps = NULL) {
  if(is.null(maps)) return(NULL)
  
  #data = c(1:length(maps[[1]]$map@data[,1]))
  data = c(1:length(maps[[1]]$rstr@data@values))
  #for(i in 1:length(myReactives$maps)) data = cbind(data,maps[[i]]$map@data)
  for(i in 1:length(myReactives$maps)) {
    data = cbind(data,maps[[i]]$rstr@data@values)
    colnames(data)[i+1] = maps[[i]]$fn
  }
  
 #browser()
  for (i in 1:length(data[1,])){
    data = data[!is.na(data[,i]),]
  }
 #browser()
  return(data)
}


drawModMapPlot <- function (maps = NULL, clusters = NULL) {
  if(is.null(maps) || is.null(data)|| is.null(clusters)) return(NULL)
  
  # get live cells
  data = getLiveMapsData(maps)
  lividx = data[,1]
  datplot = myReactives$maps[[1]]$rstr
  datplot@data@values = NA
  datplot@data@values[lividx] <- clusters
  plot(datplot,col = rainbow(max(clusters)))
}

drawGMM <- function(maps = NULL, nclass = 3) {
  if(is.null(maps)) return(NULL)
  
  data = getLiveMapsData(maps)
  
  dat = center_scale(data[,2:length(data[1,])])
  gmm <- GMM(dat,gaussian_comps = nclass,"maha_dist", "random_subset",km_iter = 10,em_iter = 10)
  
  closest <- nn2(gmm$centroids, dat)
  
  clr=nclass + 1 - closest$nn.idx[,1]
  plot(as.data.frame(data[,2:length(data[1,])]),col = rainbow(nclass)[clr], pch = 16)
  return(gmm)
}


getModelText <- function(fit = NULL) {
  if(is.null(fit)) return("No model provided" )
  names = names(attributes(fit$terms)$dataClasses)
  frm = paste(names[1],"=",prettyNum(fit$coefficients[1]))
  for( par in 2:length(fit$coefficients)) {
    frm = paste(frm,
                "+" , names(fit$coefficients)[par], 
                "*",prettyNum(fit$coefficients[par]))
  }
  #browser()
  return(frm)
}
  

getModelXplotText <- function(data = NULL, lmfit = NULL, srows = NULL) {
  sel = c( 1:length(data$WELL))
  sel[srows] = NA
  sel = sel[!is.na(data$Values)]
  sel = sel[!is.na(sel)]
  predicted = predict.lm(lmfit)
  measured = data$Values[sel]
  abl = lm(predicted~measured)
  return(paste(abl$call$formula[2]," = ", abl$call$formula[3]," * ",prettyNum(abl$coefficients[2]),"+",prettyNum(abl$coefficients[1])))
  #plot(anova(myReactives$fit))
}


drawXYplot <- function (xy = NULL, transp = 1) {
  if(is.null(xy)) return()
  xccf <- ccf(xy[[1]], xy[[2]], lag.max = 0, plot = F)
  
  #xmean <- mean(xy$x)
  xstd <- sd(xy[[1]])
  xdv <- max(xy[[1]])-min(xy[[1]])
  
  #ymean <- mean(xy$y)
  ystd <- sd(xy[[2]])
  ydv <- max(xy[[2]])-min(xy[[2]])
  
  dms <- sqrt((2*xstd)^2+(2*ystd)^2)
  dmd <- sqrt(xdv^2+ydv^2)
  dm <- abs(as.numeric(xccf$acf))*(dms/dmd)*0.5
  #mcol - expected max number of points in a cell
  #mcol <- length(xy$x)^(dm)
  mcol <- ceiling((length(xy[[1]])*0.95)^sqrt(dm))*10^(transp)
  plot(
    xy[[1]],
    xy[[2]],
    xlab = names(xy)[1],
    ylab = names(xy)[2],
    #xlab = mcol,
    #ylab = dm,
    main = sprintf("Кроссплот CC=%5.2f %s=%5.2f", xccf$acf, parse(text = 'R^2'), as.numeric(xccf$acf) ^ 2),
    col = rgb(0, 0, mcol, 1 , maxColorValue = mcol),
    pch = 16
  )
  
}

drawWellsTable <- function (wells_ = NULL) {
  if(is.null(wells_)) return()
  wells = as.data.frame(wells_)
  datatable(
    cbind(' ' = '&oplus;', wells ), escape = 0,
    caption = "Скважины: ",
    filter = "none",
    #height = 5,
    #editable = TRUE,
    class = "compact",
    rownames = FALSE,
    options = list(
      pagingType = "simple",
      paging = FALSE,
      ColumnRender = prettyNum,
      scrollY = "400px",
      scrollCollapse = TRUE,
      pageLength = 10,
      columnDefs = list(
        list(visible = FALSE, targets = c(2:3)),
        list(orderable = FALSE, className = 'details-control', targets = 0)
      )
    ),
    callback = JS("
                  table.column(1).nodes().to$().css({cursor: 'pointer'});
                  var format = function(d) {
                  return '<div style=\"background-color:#eee; padding: .5em;\"> Loc: (X:' +
                  Math.round(d[2]) + ' Y:' + Math.round(d[3]) + ')</div>';
                  };
                  table.on('click', 'td.details-control', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  td.html('&oplus;');
                  } else {
                  row.child(format(row.data())).show();
                  td.html('&CircleMinus;');
                  }
                  });"

    )
    )

}

drawMapsTable <- function (maps_ = NULL) {
  if(is.null(maps_)) return()
  
  maps = matrix(,nrow = length(maps_), ncol = 4)
  for( row in 1:length(maps_)) {
    map = maps_[[row]]
    maps[row,] <- c(paste0("Map",row),Name = map$fn, Min = map$rstr@data@min, Max = map$rstr@data@max)
  }
  #names(maps) <- ' '
  
  colnames(maps) <- c("Ref","Name","Min","Max")

  datatable(
    maps, escape = 0,
    caption = "Карты: ",
    filter = "none",
    #height = 5,
    #editable = TRUE,
    class = "compact",
    rownames = FALSE,
    options = list(
      pagingType = "simple",
      paging = FALSE,
      ColumnRender = prettyNum,
      scrollY = "400px",
      scrollCollapse = TRUE,
      pageLength = 10#,
      # columnDefs = list(
      #   list(visible = FALSE, targets = c(2:3)),
      #   list(orderable = FALSE, className = 'details-control', targets = 0)
      # )
     )#,
    # callback = JS("
    #               table.column(1).nodes().to$().css({cursor: 'pointer'});
    #               var format = function(d) {
    #               return '<div style=\"background-color:#eee; padding: .5em;\"> Loc: (X:' +
    #               Math.round(d[2]) + ' Y:' + Math.round(d[3]) + ')</div>';
    #               };
    #               table.on('click', 'td.details-control', function() {
    #               var td = $(this), row = table.row(td.closest('tr'));
    #               if (row.child.isShown()) {
    #               row.child.hide();
    #               td.html('&oplus;');
    #               } else {
    #               row.child(format(row.data())).show();
    #               td.html('&CircleMinus;');
    #               }
    #               });"
    # 
    # )
    )
  
  }

#match and return vectors for CC and xplot calc
getXYvectors <- function(map1, map2) {
  if(is.null(map1) || is.null(map2)) return(NULL)
  mask <- map1$mat + map2$mat
  mask[!is.na(mask)] <- TRUE
  mask[is.na(mask)] <- FALSE
  
  x1 = map1$mat[as.logical(mask)]
  x2 = map2$mat[as.logical(mask)]
  xy = list(x1,x2)
  names(xy) <- c(map1$fn,map2$fn)
  return (xy)
}

# Define server logic required to draw a histogram
options(shiny.maxRequestSize = 500 * 1024 ^ 2)
options(shiny.reactlog = TRUE)
options(shiny.host = "0.0.0.0")
options(shiny.port = 8080)

server <- function(input, output, session) {
  # if(is.null(myReactives$maps)) {
  #   myReactives$maps <- list(def_map,def_map)
  # }
  #CB: set Zoom rect ####
  observeEvent(input$plot_brush , {
    myReactives$zoom <- rbind(c(input$plot_brush$xmin,input$plot_brush$xmax),
                              c(input$plot_brush$ymin,input$plot_brush$ymax))
  })
  

  #CB: update upscalingmethid ####
  observeEvent(input$funcSelect1, {
    map_obj = upscaleMap(myReactives$map1,input$rstr_fact,func = flist[[input$funcSelect1]])
    observe(myReactives$map1 <- map_obj)
  })
  observeEvent(input$funcSelect2, {
    map_obj = upscaleMap(myReactives$map2,input$rstr_fact,func = flist[[input$funcSelect2]])
    observe(myReactives$map2 <- map_obj)
  })
  
  #CB: load Wells ####
  observeEvent(input$wellsfile1, {
    wls <- loadWells(input$wellsfile1)
    if (is.null(wls)) 
      showNotification(ui = "Error loading Well file.
Assume the Column-based Format:
WELL  X_LOCATION  Y_LOCATION",
                       type = "error")
    else {
      showNotification(ui = paste(length(wls[,1])," wells loaded"),
                       type = "default")
      # if(is.null(myReactives$maps) || length(myReactives$maps) == 0 ) {
      #   map_obj <- upscaleMap(def_map,input$rstr_fact,func = median)
      #   myReactives$maps <- append(myReactives$maps,list(map_obj))
      #   myReactives$maps <- append(myReactives$maps,list(map_obj))
      # }
      #browser()
      #wls <- addWellCP(wells = wls,cpdata = c(rep(NA,times = length(wls[,1]))))
      for( i in 1:length(myReactives$maps))  {
        wls <- extractMap2Well(wls,myReactives$maps[[i]]$rstr, paste0("Map",i))
      }
      myReactives$wells <- wls
    }
  })
  
  #CB: load CP ####
  observeEvent(input$cpfile1, {
    wls <- loadCP(myReactives$wells,input$cpfile1,cpname = "Values")
    if (is.null(wls)) 
      showNotification(ui = "Error loading CP file. Load Wells first.
Assume the Column-based Format:
X_LOCATION  Y_LOCATION  VALUE",
                       type = "error")
    else {
      showNotification(ui = paste(length(wls@data$Values[!is.na(wls@data$Values)])," of ",length(wls[,1])," wells updated"),
                       type = "default")
      myReactives$wells <- wls
    }
  })

  #CB: load maps list files ####
  observeEvent(input$Mapsfile , {
    map_obj = loadMapFile(input$Mapsfile)
    map_obj = upscaleMap(map_obj,input$rstr_fact,func = mean)
    
    if (is.null(map_obj)) 
       showNotification(ui = "Error loading Map file. Assume the Format should be ESRI ASCIIgrid.",
                          type = "error")
    else {
      if (length(input$table_maps_rows_selected)>0 && length(myReactives$maps)>1) {
        map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 1)
        myReactives$maps[[map_idx]] <- map_obj
        myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",map_idx))
      } else {
        myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",length(myReactives$maps)+1))
        myReactives$maps <- append(myReactives$maps,list(map_obj))
      }
    }
  })
  
  #CB: Transposing ####
  observeEvent(input$trans1 , {
    if(length(myReactives$maps)>1) {
      map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 1)
      map_obj = transposeMap(myReactives$maps[[map_idx]])
      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",map_idx))
      myReactives$maps[[map_idx]] <- map_obj
    }
  })
  observeEvent(input$trans2 , {
    if(length(myReactives$maps)>1) {
      map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 2)
      map_obj = transposeMap(myReactives$maps[[map_idx]])
      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",map_idx))
      myReactives$maps[[map_idx]] <- map_obj
    }
  })
  
  #CB: upscaling  ####
  observeEvent(input$rstr_fact , {
    for (i in 1:length(myReactives$maps)) {
      map_obj = upscaleMap(myReactives$maps[[i]],input$rstr_fact,func = median)
      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",i))
      myReactives$maps[[i]] <- map_obj
    }
    # if(length(myReactives$maps)>1) {
    #   map_idx1 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 1)
    # map_obj1 = upscaleMap(myReactives$maps[[map_idx1]],input$rstr_fact,func = median)
    # map_idx2 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 2)
    # map_obj2 = upscaleMap(myReactives$maps[[map_idx2]],input$rstr_fact,func = median)
    # myReactives$wells <- extractMap2Well(myReactives$wells,map_obj1$rstr, paste0 ("Map",map_idx1))
    # myReactives$wells <- extractMap2Well(myReactives$wells,map_obj2$rstr, paste0 ("Map",map_idx2))
    # myReactives$maps[[map_idx1]] <- map_obj1
    # myReactives$maps[[map_idx2]] <- map_obj2
    #}
  })
  
  #CB: plot maps ####
  output$mapPlot1 <- renderPlot({
    #drawMap(myReactives$map1$map,input$rstr_fact,myReactives$zoom)
    #drawRstr(myReactives$map1$rstr,myReactives$zoom)
    if(is.null(myReactives$maps) || length(myReactives$maps) == 0 ) {
      map_obj <- upscaleMap(def_map,input$rstr_fact,func = median)
      myReactives$maps <- append(myReactives$maps,list(map_obj))
      myReactives$maps <- append(myReactives$maps,list(map_obj))
    }
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 1)
    drawRstr(myReactives$maps[[map_idx]]$rstr,myReactives$zoom)
    drawWells(myReactives$wells, myReactives$maps[[map_idx]]$rstr, sr = input$table_wells_rows_selected)
  })
  output$mapPlot2 <- renderPlot({
    #drawMap(myReactives$map2$map,input$rstr_fact,myReactives$zoom)
    #drawRstr(myReactives$map2$rstr,myReactives$zoom)
    if(length(myReactives$maps)>1) {
      map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 2)
      drawRstr(myReactives$maps[[map_idx]]$rstr,myReactives$zoom)
      drawWells(myReactives$wells, myReactives$maps[[map_idx]]$rstr, sr = input$table_wells_rows_selected)
    }
  })

  #CB: Zoom reset ####
  observeEvent(input$unzoom1 , {
    myReactives$zoom <- NULL
    session$resetBrush("plot_brush")
  })
  observeEvent(input$unzoom2 , {
    myReactives$zoom <- NULL
    session$resetBrush("plot_brush")
  })
  
  #CB: plot histograms ####
  output$histPlot1 <- renderPlot({
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 1)
    drawHist(myReactives$maps[[map_idx]]$mat, input$bins)
  })
  output$histPlot2 <- renderPlot({
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 2)
    drawHist(myReactives$maps[[map_idx]]$mat, input$bins)
  })
  
  #CB: plot hexbin xplot ####
  output$xPlot_hex <- renderPlot({
    if(is.null(myReactives$maps) || length(myReactives$maps) == 0 ) {
      map_obj <- upscaleMap(def_map,input$rstr_fact,func = median)
      myReactives$maps <- append(myReactives$maps,list(map_obj))
      myReactives$maps <- append(myReactives$maps,list(map_obj))
    }
    map_idx1 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 1)
    map_idx2 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 2)
    drawHex(getXYvectors(myReactives$maps[[map_idx1]], myReactives$maps[[map_idx2]]), input$cells)
  })
  
  #CB: plot MLR model ####
  output$fitPlot <- renderPlot({
    myReactives$fit <- drawModelQC(buildMLR(myReactives$wells, input$table_wells_rows_selected))
  })
  output$fitText <- renderText({ #renderPrint
    
    fit = buildMLRmul(myReactives$wells, input$table_wells_rows_selected)
    frm = getModelText(fit)
    myReactives$fit <- fit
    paste(frm)
  })
  
  #CB: plot MLR Xplot ####
  output$fitxPlot <- renderPlot({
    fit <- buildMLRmul(myReactives$wells, input$table_wells_rows_selected)
    drawModelXplot (myReactives$wells@data, fit, input$table_wells_rows_selected)
    #myReactives$fit <- fit
    observe(myReactives$fit <- fit)
  })
  
  output$fitXText <- renderText({ #renderPrint
    fit <- buildMLRmul(myReactives$wells, input$table_wells_rows_selected)
    txt <- getModelXplotText (myReactives$wells@data, fit, input$table_wells_rows_selected)
    #myReactives$fit <- fit
    observe(myReactives$fit <- fit)
    paste(txt)
  })

  #CB: plot GLM model ####
  output$glmPlot <- renderPlot({
    fit <- buildGLM(myReactives$wells, input$table_wells_rows_selected)
    par(mfrow = c(2,2))
    plot(fit)
    observe(myReactives$fit <- fit)
  })
  output$glmText <- renderText({ #renderPrint
    fit <- buildGLM(myReactives$wells, input$table_wells_rows_selected)
    #fit = buildMLR(myReactives$wells, input$table_wells_rows_selected)
    frm = getModelText(fit)
    observe(myReactives$fit <- fit)
    paste(frm)
  })
  
  #CB: plot GLM Xplot ####
  output$glmXPlot <- renderPlot({
    fit <- buildGLM(myReactives$wells, input$table_wells_rows_selected)
    drawModelXplot (myReactives$wells@data, fit, input$table_wells_rows_selected)
    observe(myReactives$fit <- fit)
  })
  
  output$glmXText <- renderText({ #renderPrint
    fit <- buildGLM(myReactives$wells, input$table_wells_rows_selected)
    txt <- getModelXplotText (myReactives$wells@data, fit, input$table_wells_rows_selected)
    observe(myReactives$fit <- fit)
    paste(txt)
  })
  
  #CB: plot KM model ####
  output$kmPlot <- renderPlot({
    #drawKMeans(myReactives$wells@data,input$kmClasses)
    myReactives$km <- drawKMeans(myReactives$maps,input$kmClasses)
  })

  output$kmXPlot <- renderPlot({
    
    drawModMapPlot(myReactives$maps,myReactives$km$cluster)

  })
  
  #CB: plot GMM model ####
  output$gmmPlot <- renderPlot({
    #drawGMM(myReactives$wells@data,input$gmmClasses)
    #data = myReactives$maps[[1]]$map@data
    #for(map in myReactives$maps) data = cbind(data,map$map@data)
    myReactives$gmm <- drawGMM(myReactives$maps,input$gmmClasses)
  })

  output$gmmXPlot <- renderPlot({
    #drawGMM(myReactives$wells@data,input$gmmClasses)
    data = getLiveMapsData(myReactives$maps)

    closest <- nn2(myReactives$gmm$centroids, center_scale(data[,2:length(data[1,])]))
    
    clr=input$gmmClasses + 1 - closest$nn.idx[,1]
    
    drawModMapPlot(myReactives$maps,clr)

    # if(!is.null(myReactives$gmm)) {
    #   lividx = c(1:length(data[,1]))
    #   for (i in 2:length(data)){
    #     lividx = lividx[!is.na(data[i])]
    #   }
    #   datplot = myReactives$maps[[1]]$map
    #   datplot@data[] = NA
    #   datplot@data[lividx] = myReactives$gmm
    #   browser()
    #   plot(datplot)
    # }
      
  })
  
  #CB: plot simple xplot ####
  output$xPlot_simple <- renderPlot({
    map_idx1 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 1)
    map_idx2 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 2)
    drawXYplot(getXYvectors(myReactives$maps[[map_idx1]], myReactives$maps[[map_idx2]]),input$transp)
  })
  
  #CB: draw wells Table ####
  output$table_wells <- renderDataTable({
    drawWellsTable(myReactives$wells)
  })
  
  #CB: draw maps Table ####
  output$table_maps <- renderDataTable({
    drawMapsTable(myReactives$maps)
  })
  

  #CB: set Tabs names ####
  output$tab1 = renderText(({
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 1)
    basename(myReactives$maps[[map_idx]]$fn)
  }))
  output$tab2 = renderText(({
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = 2)
    basename(myReactives$maps[[map_idx]]$fn)
  }))

  #CB: print out the cursor positions   ####
  output$info <- renderText({
    xy_str <- function(e) {
      if (is.null(e))
        return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if (is.null(e))
        return("NULL\n")
      paste0(
        "xmin=",
        round(e$xmin, 1),
        " xmax=",
        round(e$xmax, 1),
        " ymin=",
        round(e$ymin, 1),
        " ymax=",
        round(e$ymax, 1)
      )
      
    }
    
    paste0(
      "click: ",
      xy_str(input$plot_click),
      "dblclick: ",
      xy_str(input$plot_dblclick),
      "hover: ",
      xy_str(input$plot_hover),
      "brush: ",
      xy_range_str(input$plot_brush)
    )
  })
  
  gc()
}

# Run the application
shinyApp(ui = ui, server = server)
