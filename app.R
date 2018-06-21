#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


require(shiny)
require(shinyjs)
require(DT)

require(readr)
require(raster)
require(miniUI)

#detach(package)

#library(spatial)
require(sp)
require(RANN)
require(RSNNS)
#require(ClusterR)
require(mclust)
require(amap)
require(hexbin)
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


source("nnet_plot_update.r") ## Acsource from https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r'

#source("app_ui.R")#, echo = F,encoding = "UTF-8" )
flist <- list(mean,median,sd)
names(flist) <- c('Среднее','Медиана','Ст.Откл.')
#names(flist) <- c("Mean","Median","StdDev")

auxList <- list("BIC","density","uncertainty")
names(auxList) <- c("Критерий информативности Байеса", 
                    "Функция плотности вероятности",
                    "Кроссплот с учетом неопределенности")
hcmModes_hclust <- list("ward.D", "ward.D2", "single", "complete", "average" , 
                 "mcquitty" , "median" , "centroid")
names(hcmModes_hclust) <- c("Ward's minimum variance (compact, spherical clusters)", 
                     "Ward's with (1963) clustering criterion", 
                     "single (‘friends of friends’ clustering strategy)", 
                     "complete (finds similar clusters)",
                     "average (somewhere between the single and complete)" , 
                     "mcquitty (somewhere between the single and complete)" ,
                     "median (not leading to a monotone distance measure)" ,
                     "centroid (not leading to a monotone distance measure)")

hcmModes_hcluster <- list("ward", "single", "complete", 
                          "average", "mcquitty", "median" ,
                          "centroid","centroid2")
names(hcmModes_hcluster) <- c("ward (compact, spherical clusters)",
                              "single (‘friends of friends’ clustering strategy)",
                              "complete (finds similar clusters)", 
                              "average (somewhere between the single and complete)",
                              "mcquitty (somewhere between the single and complete)", 
                              "median (not leading to a monotone distance measure)" ,
                              "centroid (not leading to a monotone distance measure)",
                              "centroid2 (not leading to a monotone distance measure)")

hcmDistModes <- list("euclidean", "maximum", "manhattan", "canberra", "binary", 
                     "pearson", "abspearson", "correlation", "abscorrelation", 
                     "spearman" , "kendall")
names(hcmDistModes) <- c("euclidean", "maximum", "manhattan", "canberra", "binary", 
                         "pearson", "abspearson", "correlation", "abscorrelation", 
                         "spearman" , "kendall")

mapPalList <- list (terrain.colors, heat.colors, topo.colors, 
                    rainbow, bpy.colors, gray.colors)
names(mapPalList) <- c("Покров", "Тепло", "Рельеф", 
                       "Радуга", "ЧРЖ", "Монохром")

classRange = 2:15
classPalette = mclust.options("classPlotColors")
mapPalette = terrain.colors

map_hei = "900px"
map_wid = "900px" #"900px"
hist_hei = "200px"
hist_wid = "200px"
butt_wid = "200px"
modPlot_wid = "500px"

myReactives <- reactiveValues(wells = wells0, 
                              zoom = map_zoom, 
                              fit = NULL, 
                              maps = NULL,
                              classPalette = classPalette,
                              mapPalette = mapPalette)
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Анализ карт и контрольных точек"),
  # test = enc2utf8(c("привет","пока"))
  
  #UI: Sidebar with a slider input for number of bins ####
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        #UI: Wells inputs and display ####
        tabPanel(
          "Скважины",
          fileInput(
            "wellsfile1",
            "Загрузить координаты скважин:",
            accept = '.txt',
            buttonLabel = "Открыть..."
          ),
          fileInput(
            "cpfile1",
            "Загрузить контрольные значения:",
            accept = '.txt',
            buttonLabel = "Открыть..."
          ),
          dataTableOutput('table_wells'),
          downloadButton('downloadWellsCP', 'Сохранить в файл')
        )  ,
        #UI: Maps panels  ####
        tabPanel(
          "Карты",
          # UI: Class definition ####
          conditionalPanel(
            condition = "input.maps == 'modelKM' || input.maps == 'modelGM' || input.maps == 'modelHC'",
            sliderInput(
              "numClasses",
              "Число классов:",
              min = min(classRange),
              max = max(classRange),
              step = 1,
              value = 3
            ),
            checkboxInput(
              "numClassUD",
              "Автоматически",
              FALSE
            )
          ),
          tabsetPanel( id = "maps",
                       #UI: Maps input  ####
                       tabPanel( "Данные", value = "input",
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
                                   accept = '.asc',#c("text/plain",
                                   #"text/esri-asciigrid,text/plain",
                                   #"*.asc"),
                                   buttonLabel = "Открыть...",
                                   multiple = TRUE
                                 ),
                                 dataTableOutput('table_maps'),
                                 actionButton("delmap"   , "Удалить выбранные", width = butt_wid)
                       ),
                       #UI: HexPlot display ####
                       tabPanel(  value = "xplot",  title = "Гексаплот",
                                  plotOutput(
                                    "xPlot_hex",
                                    click = "plot_zoom",
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
                       #UI: Xplot display ####
                       tabPanel(  value = "xplot",  title = "Кроссплот",
                                  plotOutput(
                                    "xPlot_simple",
                                    click = "plot_zoom",
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
                       #UI: model KM ####
                       tabPanel( "K-Means", value = "modelKM",
                                 tabsetPanel( id = "modelKM",
                                              tabPanel( "Модель", value = "mod",
                                                        plotOutput(
                                                          "kmPlot",
                                                          click = "plot_zoom",
                                                          height = modPlot_wid
                                                        ),
                                                        verbatimTextOutput("kmText")
                                              ),
                                              tabPanel( "Результат", value = "res",
                                                        plotOutput(
                                                          "kmXPlot",
                                                          click = "plot_zoom",
                                                          height = modPlot_wid
                                                        ),
                                                        downloadButton('downloadKMMap', 'Сохранить карту'),
                                                        verbatimTextOutput("kmXText")
                                              )
                                 )#,                                 verbatimTextOutput("kmXText")
                       ),
                       #UI: model GMM ####
                       tabPanel(  "GMM", value = "modelGM",
                                  tabsetPanel( id = "modelGM",
                                               tabPanel( "Модель", value = "mod",
                                                         plotOutput(
                                                           "gmmPlot",
                                                           click = "plot_zoom",
                                                           height = modPlot_wid
                                                         ),
                                                         verbatimTextOutput("gmmText")
                                               ),
                                               tabPanel( "Результат", value = "res",
                                                         plotOutput(
                                                           "gmmXPlot",
                                                           click = "plot_zoom",
                                                           height = modPlot_wid
                                                         ),
                                                         downloadButton('downloadGMMap', 'Сохранить карту'),
                                                         verbatimTextOutput("gmmXText")
                                               ),
                                               tabPanel( "Дополнительно", value = "aux",
                                                         plotOutput(
                                                           "gmAuxPlot",
                                                           click = "plot_zoom",
                                                           height = modPlot_wid
                                                         ),
                                                         radioButtons("gmmAuxMode",
                                                           label = "",
                                                           choices = auxList
                                                         )
                                               )
                                  )#,                                  verbatimTextOutput("gmmXText")
                       ),
                       #UI: model HC ####
                       tabPanel(  "Иерархич.", value = "modelHC",
                                  tabsetPanel( id = "modelHC",
                                               tabPanel( "Модель", value = "mod",
                                                         plotOutput(
                                                           "hcPlot",
                                                           click = "plot_zoom",
                                                           height = modPlot_wid
                                                         ),
                                                         selectInput("hcMode", "Метод аггломерации"
                                                                     ,choices = names(hcmModes_hcluster), width = modPlot_wid
                                                         ),
                                                         selectInput("hcDistMode", "Метод расчета расстояний"
                                                                     ,choices = names(hcmDistModes), width = modPlot_wid
                                                         ),
                                                         verbatimTextOutput("hcText")
                                               ),
                                               tabPanel( "Результат", value = "res",
                                                         plotOutput(
                                                           "hcXPlot",
                                                           click = "plot_zoom",
                                                           height = modPlot_wid
                                                         ),
                                                         downloadButton('downloadHCmap', 'Сохранить карту'),
                                                         verbatimTextOutput("hcXText")
                                               )
                                  )#,                                  verbatimTextOutput("hcXText")
                       )
          )),
        #UI: models ####
        tabPanel(
          "Модели",
          tabsetPanel(
            #UI: model NNET ####
            tabPanel(
              "NNET",
              sliderInput(
                "nnet_complex",
                "Сложность сети, %:",
                min = 10,
                max = 90,
                value = 10
              ),
              sliderInput(
                "test_ratio",
                "Размер тестовой выборки, %:",
                min = 10,
                max = 90,
                value = 30
              ),
              sliderInput(
                "max_iter",
                "Число итераций:",
                min = 10,
                max = 500,
                value = 50
              ),
              plotOutput(
                "nnetPlot",
                height = modPlot_wid
              ),
              verbatimTextOutput("nnetText"),
              plotOutput(
                "nnetxPlot",
                height = modPlot_wid
              ),
              verbatimTextOutput("nnetXText")
            ),
            #UI: model GLM ####
            tabPanel(
              "GLM",
              plotOutput(
                "glmPlot",
                height = modPlot_wid
              ),
              verbatimTextOutput("glmText"),
              plotOutput(
                "glmXPlot",
                height = modPlot_wid
              ),
              verbatimTextOutput("glmXText")
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
                  selectInput("selectMap1", "Карта"
                              ,choices = "", width = butt_wid
                  )
                ),
                flowLayout(
                  actionButton("unzoom1"   , "Сброс масштаба  ", width = butt_wid),
                  actionButton("trans1"    , "Транспонировать ", width = butt_wid),
                  checkboxInput("showContours1","Контуры", width = butt_wid),
                  selectInput("mapPalSelect1", "Палитра"
                              ,choices = names(mapPalList), width = butt_wid),
                  sliderInput(
                    "mapPalTransp1",
                    "Прозрачность:",
                    min = 0.1,
                    max = 1,
                    value = 0.5,
                    step = 0.1)
                  
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
                  selectInput("selectMap2", "Карта"
                              ,choices = "", width = butt_wid
                  )
                ),
                flowLayout(
                  actionButton("unzoom2"   , "Сброс масштаба  ", width = butt_wid),
                  actionButton("trans2"    , "Транспонировать ", width = butt_wid),
                  checkboxInput("showContours2","Контуры", width = butt_wid),
                  selectInput("mapPalSelect2", "Палитра"
                              ,choices = names(mapPalList), width = butt_wid),
                  sliderInput(
                    "mapPalTransp2",
                    "Прозрачность:",
                    min = 0.1,
                    max = 1,
                    value = 0.5,
                    step = 0.1)
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
  map_obj$rstr@data@values = as.vector(matrix(map_obj$rstr@data@values, ncol = c, nrow = r, byrow = TRUE))
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
  ##projection(map) <- "+proj=longlat +datum=WGS84"
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

## delete maps
deleteMaps <- function (maps = NULL, wells = NULL, sr = NULL) {
  if(is.null(sr) || length(sr)<1) return(maps)
  
  i=length(sr)
  while (i) {
    maps[[sr[i]]] = NULL
    i=i-1
  }
  #browser()
  #wls <- addWellCP(wells = wls,cpdata = c(rep(NA,times = length(wls[,1]))))
  if(length(maps) <1 ) return(list(maps = NULL,wells = wells))
  
  if(is.null(wells) || length(wells)<1) return(list(maps = maps,wells = wells))
  
  wls = data.frame(wells@data$WELL,wells@coords,wells@data$Values)
  colnames(wls) = c("WELL","X_LOCATION","Y_LOCATION","Values")
  coordinates(wls) = ~X_LOCATION+Y_LOCATION
  for( i in 1:length(maps))  {
    wls <- extractMap2Well(wls,maps[[i]]$rstr, paste0("Map",i))
  }
  return(list(maps = maps,wells = wls))
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

dbgmes <- function (message = "message", expr = NULL) {
  #browser()
  funame = unlist(strsplit(x = as.character(sys.calls()[length(sys.calls())-1]),split = "\\("))
  cat(paste0('==>',funame[1],':\n',paste0(message,':',capture.output(expr),'\n')))
}

sortClasses <- function (rstr = NULL, wells = NULL) {
  if(is.null(rstr) || 
     is.null(wells) || 
     length(wells$Values[!is.na(wells$Values)])<1)
    return(rstr)
  
  clsAtWells = data.frame( Values = wells$Values, 
                  iClasses = extract(rstr,data.frame(wells$X_LOCATION,wells$Y_LOCATION)))
  ##??? - what is the limit for the actual well hits ??? ####
  if(length(clsAtWells$iClasses[is.na(clsAtWells$iClasses)]) < 2)
    return(rstr)
  
  ncls=max(rstr@data@values,na.rm = T)
  cls = data.frame ( iClasses = c(1:ncls),Values = rep(NA,times = ncls))
  for (icls in 1:ncls) {
    vals=clsAtWells$Values[clsAtWells$iClasses==icls]
    vals = vals[!is.na(vals)]
    #browser()
    if(length(vals)>1)
    {
      dens = density(vals,na.rm=T)
      cls$Values[icls] = dens$x[which.max(dens$y)]
    } else if( length(vals)==1 ) {
      cls$Values[icls] = vals
    }
    else 
      cls$Values[icls] = NA
    #dbgmes("icls=",vals)
  }
  
  #cls_out = cls$iClasses[!is.na(cls$Values)]
  #dbgmes("in",cls)
  
  cls_sorted = sort(cls$Values,na.last=F)
  cls = cls[append(which(is.na(cls$Values)),match(cls_sorted[!is.na(cls_sorted)],cls$Values)),]
  #dbgmes("out",cls)
  #dbgmes("cls_out",cls_out)
  if(length(cls)>0) {
    values = rep(NA,times = length(rstr@data@values))
    for(icls in 1:ncls) {
      idxs = which(rstr@data@values == cls$iClasses[icls])
      values[idxs] = replace(rstr@data@values,idxs,icls)[idxs]
    }
    rstr@data@values = values
  }
  #browser()
  rownames(cls)<-as.character(c(1:ncls))
  attributes(rstr)$cls = cls
  return(rstr)
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
drawMap <- function (map = def_map$map, fact = 1.0, zoom = NULL, pal = NULL, alpha = 0) {
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
  colors = pal(128)
  if(!is.null(alpha))
    colors = setPaletteTransp(colors,1-alpha)
  
  if(!is.null(zoom)) {
    plot(map, xlim = zoom[1,], ylim = zoom[2,],col = colors)
  } 
  else {
    zoom <- NULL
    par(new = TRUE)
    plot(map,col = colors)
  }
}

selectMap <- function (maps = NULL, sr = NULL, idx = -1) {
  #browser()
  #dbgmes(message = " in_idx=",idx)
  idx = as.integer(idx)
  if(is.null(maps)) return (NULL)
  if(is.null(idx) || is.na(idx) || idx<1 || idx > length(maps)) return (1) 
  
  #if ( is.null(sr) || length(sr) < 1 ) map_obj = maps[[1]]
  #else map_obj = maps[[sr[1]]]
  
  if(length(sr) == 1) return(sr[1])
  
  if ( is.null(sr) || length(sr) < 1 ) map_idx = idx
  else map_idx = idx
  #map_idx = as.integer(map_idx)
  #if(is.na(map_idx)) browser()
  #dbgmes(message = "out_idx=",map_idx)
  return(map_idx)
}

drawRstr <- function (map = def_map$rstr, zoom = NULL,  pal = mapPalette, alpha = 0, contours = F) {
  colors = pal(128)
  if(!is.null(alpha))
    colors = setPaletteTransp(colors,1-alpha)

  if(!is.null(zoom)) {
    plot(map, xlim = zoom[1,], ylim = zoom[2,],col = colors)
  } 
  else {
    zoom <- NULL
    par(new = TRUE)
    plot(map,interpolate=TRUE,col = colors)
    if(contours) contour(map,add=T)
  }
}

drawWells <- function(wells = NULL, rstr = NULL, sr = NULL, srmap = NULL) {
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
    m1idx = ifelse(length(srmap)>=1,srmap[1]+2,3)
    m2idx = ifelse(length(srmap)>=2,srmap[2]+2,4)
    #browser()
    drawParTxt(wells,2,3, sr) # Value
    drawParTxt(wells,m1idx,2, sr) # Map1
    drawParTxt(wells,m2idx,4, sr) # Map2
    
    
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
             #xlab = names(xy)[1],
             #ylab = names(xy)[2],
             #aspect = "iso",
             main = sprintf("Кроссплот CC=%5.2f %s=%5.2f", xccf$acf, parse(text = 'R^2'), as.numeric(xccf$acf) ^ 2),
             xbins = cells
             #colramp = rainbow
             
  )
  #text(c(1,1),labels = basename(myReactives$map1$fn))
}


buildNNET <- function(wells = NULL, rows = NULL, sel_maps = NULL, test_ratio = 0.25, max_iter = 100, nnet_complex = 0.1){
  if(is.null(wells) || length(wells@data[1,])<4) return(NULL)
  
  if(is.null(sel_maps) || length(sel_maps)<1)
    data = wells@data
  else {
    data = data.frame(wells@data[,1:2],wells@data[,2+sel_maps])
    colnames(data) = c("WELL","Values",names(wells@data[2+sel_maps]))
  }

  row.names(data) = wells$WELL
  if(!is.null(rows)) {
    data[rows,] = NA
  }
  for (i in 1:length(data[1,])){
    data = data[!is.na(data[,i]),]
  }
  size = max(1,ceiling(sqrt(length(data[,1]))*nnet_complex*5*2))
  layers = max(1,ceiling(log(base = 5, size)))
  
  
  #browser()
  dset = splitForTrainingAndTest(x = data[,3:length(data[1,])],y = as.matrix(data$Values), ratio = test_ratio)
  nninp = dset$targetsTrain
  dset = normTrainingAndTestSet(dset,dontNormTargets = FALSE)
  nnet = mlp(dset$inputsTrain, dset$targetsTrain, 
             size = ceiling(seq.int(size/2,3, length.out = layers)), 
             learnFuncParams = c(0.5),maxit = max_iter,
             inputsTest = dset$inputsTest, targetsTest = dset$targetsTest)
  # nnet = mlp(y = as.matrix(data$Values), 
  #            x = as.matrix(data[,3:length(data[1,])]), 
  #            size = c(sqrt(length(data$Values))),
  #            learnFuncParams=c(0.1))
  nnout = predict( nnet, inp = dset$inputsTrain)  
  nnout = denormalizeData(nnout,getNormParameters(dset$targetsTrain))
  return(list(net = nnet,dset = dset, out = nnout, inp = nninp))
}

buildGLM <- function(wells = NULL, rows = NULL, sel_maps = NULL, lmfunc = glm, family = gaussian){
  if(is.null(wells) || length(wells@data[1,])<4) return(NULL)
  
  if(is.null(sel_maps) || length(sel_maps)<1)
    data = wells@data
  else {
    data = data.frame(wells@data[,2],wells@data[,2+sel_maps])
    colnames(data) = c("Values",names(wells@data[2+sel_maps]))
  }
  #browser()
  for (par in  1:length(data) ) {
    if (length(data[!is.na(data[[par]]),par]) <1) return(NULL)
  }
  
  row.names(data) = wells$WELL
  parnames = colnames(data)[2:length(data)]
  frml = as.formula(paste("Values~", paste(parnames,collapse = "+")))
  if(is.null(rows)) fit = lmfunc(formula = frml, data, family = family)
  else {
    sel = rep( TRUE, times = length(wells$WELL))
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

calcKMeans <- function(data = NULL, nclass = 3) {
  if(is.null(data)) return(NULL)

  #data = getLiveMapsData(maps,sr)
  kmns = kmeans(scale(as.matrix(data[,2:length(data[1,])])),nclass)
  #kmns = kmeans(data[,2:length(data[1,])],nclass)
  clr=kmns$cluster
  #plot(datplot,col = mclust.options("classPlotColors")[1:max(clusters)])
  return(kmns)
}

getLiveMapsIds <- function (maps = NULL, sr = NULL) {
  if(is.null(maps)) return(NULL)
  nsr = length(sr)
  #browser()
  if(is.null(sr) || nsr<1) {
    sr=c(1:length(maps)) 
  } 
  if(nsr == 1) sr = c(sr,sr)
  #dbgmes(message = "\tsr=",sr)
  return(sr)
}

getLiveMapsData <- function (maps = NULL, sr = NULL) {
  if(is.null(maps)) return(NULL)
  #browser()
  if(is.null(sr) || length(sr)<2) {
    sr=c(1:length(maps)) 
  } 
  #browser()
  data = c(1:length(as.vector(maps[[sr[1]]]$rstr@data@values)))
  for(i in 1:length(sr)) {
    if(length(as.vector(maps[[sr[1]]]$rstr@data@values)) != length(as.vector(maps[[sr[i]]]$rstr@data@values)))
      return(NULL)
    data = data.frame(data,as.vector(maps[[sr[i]]]$rstr@data@values))
    colnames(data)[i+1] = maps[[sr[i]]]$fn
  }
  #browser()
  for (i in 1:length(data[1,])){
    data = data[!is.na(data[,i]),]
  }
 #browser()
  return(data)
}

drawModel <- function(data,model) {
  #browser()
  if(class(model) == 'Mclust') 
    plot(model, 
         #main = (capture.output(summary(model)))[2], 
         what = "classification")
  else if(class(model) == 'kmeans') {
    plot(as.data.frame(data[,2:length(data[1,])]),
         main = (capture.output(model))[1],
         col = mclust.options("classPlotColors")[model$cluster], pch = 16)
    #if(class(model) == 'kmeans')
    #  points(model$centers,pch = 4, cex = 4, lwd = 4)
  }
  else if(class(model) == 'hclust') {
    plot(model)
  }
}

dmode <- function(x, ...) {
  dx <- density(x, ...)
  dx$x[which.max(dx$y)]
} 

fill.na <- function(x) {
  #browser()
  i= floor(length(x)/2)
  if( is.na(x)[i] && length(x[!is.na(x)]) > 2 ) {
    return( round(median(x, na.rm=TRUE),0) )
  } else {
    return( round(x[i],0) )
  }
}  

setPaletteTransp <- function(colors = NULL ,alpha = 0.5) {
  if(is.null(colors)) return(NULL)
  
  colors = apply(sapply(colors, col2rgb)/255, 2, 
                 function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 
  #cat(capture.output(colors))
  return(colors)
}
  

drawModMapPlot <- function (data = NULL, model = NULL, sr = NULL, zoom = NULL, nClass = 3) {
  if(is.null(data) || is.null(data)|| is.null(model)) return(NULL)
  msize = 1
  if(class(model) == 'Mclust') {
    title = (capture.output(summary(model)))[2]
    clusters = model$classification
  } else if(class(model) == 'kmeans') {
    title = (capture.output(model))[1]
    clusters = model$cluster
  } else if(class(model) == 'hclust') {
    title = (capture.output(model))[1]
    if(is.null(model$ids))
      clusters = cutree(model,min(nClass,length(model$height)))
    else {
      clusters = rep(0,times = length(data[,1]))
      clusters[model$ids] = cutree(model,min(nClass,length(model$height)))
      msize = floor(model$reduceFactor/2)*2+1
    }
    title = paste("reduceFactor = ",model$reduceFactor,"msize =", msize)
    #clusters = model$cluster
  }
  lividx = data[,1]
  if(!is.null(sr) && length(sr)>1) 
    datplot = myReactives$maps[[sr[1]]]$rstr
  else
    datplot = myReactives$maps[[1]]$rstr
  names(datplot) = title
  datplot@data@values = NA
  datplot@data@values[lividx] <- clusters
  datplot@data@values[datplot@data@values==0] <- NA
  
  if(msize!=1)
    datplot <- focal(datplot, w = matrix(1,msize,msize), fun = fill.na, 
                pad = TRUE, na.rm = FALSE , NAonly = T)
  datplot = sortClasses(datplot,myReactives$wells)

  colors = mclust.options("classPlotColors")[1:max(clusters)]
  #browser()
  colors = setPaletteTransp(colors,0.5)
  
  if(is.null(zoom))
    plot(datplot,
         main = title,
         col = colors ,interpolate=F)
  else
    plot(datplot,
         main = title,
         xlim = zoom[1,], ylim = zoom[2,],
         col = colors,interpolate=F)
  return (datplot)
}

getModelMapText <- function (model_map = NULL) {
  if(is.null(model_map)) return("")
  cls = attributes(model_map)$cls
  txt_gen = paste0(capture.output(model_map),"\n")
  txt_cls =""
  if(!is.null(cls))
    txt_cls = paste0(capture.output(cls),"\n")
  return(c(txt_gen,txt_cls))
}  

kmeansIC = function(fit){
  
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(data.frame(AIC = D + 2*m*k,
                    BIC = D + log(n)*m*k))
}

drawModBIC <- function(model=NULL,mode = "BIC") {
  #browser()
  if(class(model)=='Mclust') 
    plot(model,what = mode,main = (capture.output(summary(model)))[2])
  else if( class(model)=='kmeans') {
    plot(kmeansIC(model))
  }
  
}

saveModMap <- function (data = NULL, clusters = NULL, sr = NULL) {
  if(is.null(data) || is.null(data)|| is.null(clusters)) return(NULL)
  # got live cells
  lividx = data[,1]
  if(!is.null(sr) && length(sr)>1) datplot = myReactives$maps[[sr[1]]]$rstr
  else datplot = myReactives$maps[[1]]$rstr
  datplot@data@values[lividx] <- clusters
  datplot_out=datplot
  dxCellsize=(datplot@extent@xmax-datplot@extent@xmin)/datplot@nrows
  dyCellsize=(datplot@extent@ymax-datplot@extent@ymin)/datplot@ncols
  newCellSize = min(dxCellsize,dyCellsize)
  datplot_out@ncols = as.integer(ceiling((datplot@extent@ymax-datplot@extent@ymin)/newCellSize))
  datplot_out@nrows = as.integer(ceiling((datplot@extent@xmax-datplot@extent@xmin)/newCellSize))
  datplot_out = resample(datplot,datplot_out)
  spgrid = as(datplot_out,'SpatialGridDataFrame') 
  spgrid@grid@cellsize = rep(newCellSize,2)
  #browser()
  return (spgrid)
}


calcGMM <- function(data = NULL, nclass = 3) {
  if(is.null(data)) return(NULL)

  dat = scale(data[,2:length(data[1,])])
  #names(dat) = names(data[,2:length(data[1,])])
  if(nclass > 0) {
    gmmBIC = mclustBIC(dat,G=nclass)
    gmm <- Mclust(dat,x = gmmBIC,G=nclass)
  } else {
    gmmBIC=mclustBIC(dat,G=classRange)
    gmm <- Mclust(dat,x = gmmBIC)
  }

  return(gmm)
}

calcHC <- function(data = NULL, nclass = 3, mode = "complete", distMode = "euclidean", maxPairs = 16000) {
  if(is.null(data)) return(NULL)
  #browser()
  dat = data[,2:length(data[1,])]
  #names(dat) = names(data[,2:length(data[1,])])
  # usage of hclust require dist matrix which size would exceed the available memory
  # in most cases
  # hcluster requires less memory, but still requires the data decimation by reduceFactor.
  #reduceFactor = as.integer(max(1,length(dat[,1])/65000L^(1/(length(dat[1,])-1))))
  reduceFactor = ceiling(length(dat[,1])/maxPairs)
  if(reduceFactor > 1 ) {
    ids = seq(1,length(dat[,1]),reduceFactor)
    dat_= dat[ids,]
    hcm = hcluster(dat_,method = distMode, link = mode)
    #dat_$class=cutree(hcm,min(nclass,length(hcm$height)))
    #kmn=kmeans(dat,centers = dat_)
    #hcm$cluster=kmn$cluster
    hcm$ids = ids
    hcm$reduceFactor = reduceFactor
    #hcm$cluster = rep(0,times = length(dat[,1]))
    #hcm$cluster[ids] = cutree(hcm,min(nclass,length(hcm$height)))
    
  } else {
    hcm = hcluster(dat,method = distMode, link = mode)
    hcm$ids = NULL
    #hcm$cluster = cutree(hcm,min(nclass,length(hcm$height)))
  }
  return(hcm)
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
  if(is.null(data)) {
    return(paste(lmfit$call$formula[2]," = ", lmfit$call$formula[3]," * ",prettyNum(lmfit$coefficients[2]),"+",prettyNum(lmfit$coefficients[1])))  } 
  if(is.null(lmfit)) return("No model built")
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

drawMapsTable <- function (maps_ = NULL, sr = NULL) {
  if(is.null(maps_)) return()
  
  #sr = input$table_maps_rows_selected
  maps = matrix(,nrow = length(maps_), ncol = 4)
  for( row in 1:length(maps_)) {
    map = maps_[[row]]
    maps[row,] <- c(paste0("Map",row),Name = map$fn, Min = map$rstr@data@min, Max = map$rstr@data@max)
  }
  #names(maps) <- ' '
  
  colnames(maps) <- c("Ref","Name","Min","Max")

  datatable(
    maps, escape = 0,
    caption = "Доступные карты: ",
    filter = "none",
    #height = 5,
    #editable = TRUE,
    class = "compact",
    rownames = FALSE,
    selection = list (
      mode = 'multiple',
      selected = sr,
      target = 'row'
    ),
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
#options(shiny.reactlog = TRUE)
options(shiny.host = "0.0.0.0")
options(shiny.port = 8080)
#options(shiny.style="old")

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

    #CB: Wells and CP download  ####
  output$downloadWellsCP <- downloadHandler(
    filename = function() {
      paste0('Wells+CP_',Sys.Date(), '.txt')
    },
    contentType = '.txt',
    content = function (fname) {
      #browser()
      if(length(input$table_maps_rows_selected)>0) {
        map_idx1 = selectMap(maps = myReactives$maps,
                             sr = input$table_maps_rows_selected, idx = input$selectMap1)
        map_idx2 = selectMap(maps = myReactives$maps,
                             sr = input$table_maps_rows_selected, idx = input$selectMap2)        
        out_wells = cbind( myReactives$wells@coords, 
                           myReactives$wells[,map_idx1+3]@data,
                           myReactives$wells[,map_idx2+3]@data)              
        map_idxs = cbind(map_idx1,map_idx2)
      } else {
        map_idxs = 1:length(myReactives$maps)
        out_wells = cbind( myReactives$wells@coords, 
                           myReactives$wells[,3:dim(myReactives$wells)[2]]@data)
      }
      for(i in 1:length(map_idxs)) {
        names(out_wells)[i+2] = myReactives$maps[[map_idxs[i]]]$fn
      }
      #outgrid = saveModMap(myReactives$liveMaps,myReactives$gmm$classification,sr = input$table_maps_rows_selected)
      save = try( expr = write.table(x = out_wells,
                                    file = fname,
                                    sep = '    \t',
                                    dec = '.',
                                    row.names = F,
                                    col.names = F) 
                  , TRUE)
      if(class(save)=="try-error")
        showNotification(ui = "Ошибка при сохранении файла",
                         type = "error")      
    }
  )
  
  #CB: load maps list files ####
  observeEvent(input$Mapsfile , {
    #browser()
    showModal(modalDialog( "Обработка карт...",title = "Ожидайте...", footer = modalButton("Закрыть")))
    withProgress(message = "обработка карт...", detail = "Ожидайте......", 
                 value =0, {
                for(i in 1:length(input$Mapsfile[,1])) {
                  map_obj = loadMapFile(input$Mapsfile[i,])
                  map_obj = upscaleMap(map_obj,input$rstr_fact,func = mean)
                  
                  if (is.null(map_obj)) 
                     showNotification(ui = "Ошибка при загрузке. Ожидаемый формат - ESRI ASCIIgrid.",
                                        type = "error")
                  else {
                    incProgress(detail = paste0('"',map_obj$fn,'"'), amount = 1./length(input$Mapsfile[,1]))
                    if (length(input$table_maps_rows_selected)>0 && length(myReactives$maps)>1) {
                      map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap1)
                      myReactives$maps[[map_idx]] <- map_obj
                      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",map_idx))
                    } else {
                      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",length(myReactives$maps)+1))
                      myReactives$maps <- append(myReactives$maps,list(map_obj))
                    }
                  }
                }
              })
    myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps)
    updateMapLists(myReactives$maps,input$table_maps_rows_selected)
    removeModal()
  })
  
  #CB: delete maps ####
  observeEvent(input$delmap , {
    #myReactives$maps[[input$table_maps_rows_selected]] = NULL
    #browser()
    
    res = deleteMaps(myReactives$maps,myReactives$wells,sr = input$table_maps_rows_selected)
    myReactives$wells <- res$wells
    myReactives$maps <- res$maps
    myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps)
    updateMapLists(myReactives$maps)
  })
  
  #CB: select maps ####
  updateMapLists <- function (maps = NULL, sr = NULL) {
    #browser()
    sr = getLiveMapsIds(maps, sr)
    nsr=length(sr)
    for(im in 1:nsr)  
      names(sr)[im]=maps[[sr[im]]]$fn
    
    #sel1=as.integer(input$selectMap1)
    #sel2=as.integer(input$selectMap2)
    #dbgmes(message = "selected_in = ",c(sel1,sel2))
    #if(is.null(sel1) || is.na(sel1)|| sel1 == "") sel1 = NULL
    #if(is.null(sel2) || is.na(sel2)|| sel2 == "") sel2 = NULL
    updateSelectInput(session,"selectMap1",choices = c(sr))#,selected = sel1)
    updateSelectInput(session,"selectMap2",choices = c(sr))#,selected = sel2)
    #dbgmes(message = "choices = ",sr)
    #dbgmes(message = "selected_out = ",c(sel1,sel2))
  }
  
  #CB: MapTable select ####
  observeEvent(input$table_maps_rows_selected, {
    #browser()
    #dbgmes(message = "selected = ",input$table_maps_rows_selected)
    myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
    updateMapLists(maps = myReactives$maps,sr = input$table_maps_rows_selected)
    
  })

  #CB: transposing ####
  observeEvent(input$trans1 , {
    if(length(myReactives$maps)>1) {
      map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap1)
      map_obj = transposeMap(myReactives$maps[[map_idx]])
      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",map_idx))
      myReactives$maps[[map_idx]] <- map_obj
      myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps)
    }
  })
  observeEvent(input$trans2 , {
    if(length(myReactives$maps)>1) {
      map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap2)
      map_obj = transposeMap(myReactives$maps[[map_idx]])
      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",map_idx))
      myReactives$maps[[map_idx]] <- map_obj
      myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps)
    }
  })
  
  #CB: upscaling  ####
  observeEvent(input$rstr_fact , {
    showModal(modalDialog( "Обработка карт...",title = "Ожидайте...", footer = modalButton("Закрыть")))
    withProgress(message = "Обработка карт...", detail = "Ожидайте...",  value =0, {
    for (i in 1:length(myReactives$maps)) {
      map_obj = upscaleMap(myReactives$maps[[i]],input$rstr_fact,func = median)
      incProgress(detail = paste0('"',map_obj$fn,'"'), amount = 1./length(myReactives$maps))
      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",i))
      myReactives$maps[[i]] <- map_obj
      myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps)
    }
      })
    removeModal()
  })
  
  #CB: plot maps ####
  output$mapPlot1 <- renderPlot({
    if(is.null(myReactives$maps) || length(myReactives$maps) == 0 ) {
      map_obj <- upscaleMap(def_map,input$rstr_fact,func = median)
      myReactives$maps <- append(myReactives$maps,list(map_obj))
      myReactives$maps <- append(myReactives$maps,list(map_obj))
      myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps)
      updateMapLists(myReactives$maps,input$table_maps_rows_selected)
    }
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap1)
    #browser()
    drawRstr(map = myReactives$maps[[map_idx]]$rstr,zoom = myReactives$zoom, 
             pal = mapPalList[[input$mapPalSelect1]],
             alpha = input$mapPalTransp1,
             contours = input$showContours1)
    drawWells(myReactives$wells, myReactives$maps[[map_idx]]$rstr, sr = input$table_wells_rows_selected,srmap = input$table_maps_rows_selected)
  })
  output$mapPlot2 <- renderPlot({
    if(length(myReactives$maps)>1) {
      map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap2)
      drawRstr(myReactives$maps[[map_idx]]$rstr,myReactives$zoom, 
               pal = mapPalList[[input$mapPalSelect2]],
               alpha = input$mapPalTransp2,
               contours = input$showContours2)
      drawWells(myReactives$wells, myReactives$maps[[map_idx]]$rstr, sr = input$table_wells_rows_selected,srmap = input$table_maps_rows_selected)
    }
  })

  
  output$selectMap1 <- renderText({
    #updateMapLists(myReactives$maps,input$table_maps_rows_selected)
  })
  output$selectMap2 <- renderText({
    #updateMapLists(myReactives$maps,input$table_maps_rows_selected)
  })

  #CB: zoom reset ####
  observeEvent(input$unzoom1 , {
    myReactives$zoom <- NULL
    session$resetBrush("plot_brush")
  })
  observeEvent(input$unzoom2 , {
    myReactives$zoom <- NULL
    session$resetBrush("plot_brush")
  })
  
  #CB: histogram plot  ####
  output$histPlot1 <- renderPlot({
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap1)
    drawHist(myReactives$maps[[map_idx]]$mat, input$bins)
  })
  output$histPlot2 <- renderPlot({
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap2)
    drawHist(myReactives$maps[[map_idx]]$mat, input$bins)
  })
  
  #CB: hexbin plot xplot ####
  output$xPlot_hex <- renderPlot({
    if(is.null(myReactives$maps) || length(myReactives$maps) == 0 ) {
      map_obj <- upscaleMap(def_map,input$rstr_fact,func = median)
      myReactives$maps <- append(myReactives$maps,list(map_obj))
      myReactives$maps <- append(myReactives$maps,list(map_obj))
    }
    map_idx1 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap1)
    map_idx2 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap2)
    drawHex(getXYvectors(myReactives$maps[[map_idx1]], myReactives$maps[[map_idx2]]), input$cells)
  })
  
  #CB: NNET plot model ####
  output$nnetPlot <- renderPlot({
    showModal(modalDialog( "Обучение нейронной сети MLP...",title = "Ожидайте...", footer = modalButton("Закрыть")))
    myReactives$nnet <- buildNNET(myReactives$wells, 
                                  input$table_wells_rows_selected, input$table_maps_rows_selected,
                                  test_ratio = input$test_ratio/100.,
                                  max_iter = input$max_iter,
                                  nnet_complex = input$nnet_complex/100.)
    #browser()
    #plot(myReactives$nnet$inp,myReactives$nnet$out)
    par(mfrow=c(2,2))
    
    plotRegressionError(myReactives$nnet$dset$targetsTrain,myReactives$nnet$net$fitted.values)
    plotIterativeError(myReactives$nnet$net)
    plotROC(fitted.values(myReactives$nnet$net), myReactives$nnet$dset$targetsTrain)
    #plotROC(itted.values(myReactives$nnet$net), myReactives$nnet$inp)
    #plot.nnet(myReactives$nnet$net,col = rainbow(length(myReactives$nnet$net$neurons)))
    plot(myReactives$nnet$inp,myReactives$nnet$out)
    par(new = TRUE)
    #browser()
    lmr = lm(formula = myReactives$nnet$out~myReactives$nnet$inp)
    abline(lmr)
    #plot.nnet(myReactives$nnet$net)
    removeModal()
  })
  output$nnetText <- renderText({ #renderPrint
    #frm = getModelText(myReactives$nnet)
    mesured = myReactives$nnet$inp
    predicted = myReactives$nnet$out
    lmr = lm(formula = predicted~mesured)
    frm = getModelXplotText(lmfit = lmr)
    paste(frm)
  })
  
  #CB: NNET plot Xplot ####
  output$nnetxPlot <- renderPlot({
    plot.nnet(myReactives$nnet$net)
    #plot(myReactives$nnet$inp,myReactives$nnet$out)
  })
  
  output$nnetXText <- renderText({ #renderPrint
    #txt <- getModelXplotText (myReactives$wells@data, myReactives$nnet, input$table_wells_rows_selected)
    #paste(txt)
  })

  #CB: GLM plot model ####
  output$glmPlot <- renderPlot({
    showModal(modalDialog( "Создание модели многомерной линейной регрессии...",title = "Ожидайте...", footer = modalButton("Закрыть")))
    fit <- buildGLM(myReactives$wells, input$table_wells_rows_selected, input$table_maps_rows_selected)
    par(mfrow = c(2,2))
    plot(fit)
    removeModal()
    observe(myReactives$fit <- fit)
  })
  output$glmText <- renderText({ #renderPrint
    frm = getModelText(myReactives$fit)
    paste(frm)
  })
  
  #CB: GLM plot Xplot ####
  output$glmXPlot <- renderPlot({
    drawModelXplot (myReactives$wells@data, myReactives$fit, input$table_wells_rows_selected)
  })
  
  output$glmXText <- renderText({ #renderPrint
    txt <- getModelXplotText (myReactives$wells@data, myReactives$fit, input$table_wells_rows_selected)
    paste(txt)
  })
  
  recalcKMeans <- reactive({
    showModal(modalDialog( "Кластеризация K-means...",title = "Ожидайте...", footer = modalButton("Закрыть")))
    myReactives$km <- calcKMeans(myReactives$liveMaps,input$numClasses)#,sr = input$table_maps_rows_selected)
    removeModal()
    return(myReactives$hcm)
  })
  
  #CB: KM plot model ####
  output$kmPlot <- renderPlot({
    recalcKMeans()
    drawModel(myReactives$liveMaps,myReactives$km)
  })

  output$kmText <- renderText({ #renderPrint renderText
    paste0(capture.output(myReactives$km),"\n")
  })

  output$kmXPlot <- renderPlot({
    recalcKMeans()
    myReactives$km_map = drawModMapPlot(myReactives$liveMaps,myReactives$km,sr = input$table_maps_rows_selected)
    par(new = TRUE)
    drawWells(wells = myReactives$wells)
  })

  output$kmXText <- renderText({ #renderPrint renderText
    getModelMapText(myReactives$km_map)
  })
  
  #CB: KM download  ####
  output$downloadKMMap <- downloadHandler(
    filename = function() {
      paste0('KMeans_Ncls',input$numClasses,'_',Sys.Date(), '.asc')
    },
    contentType = '.asc',
    content = function (fname) {
      #browser()
      outgrid = saveModMap(myReactives$liveMaps,myReactives$km$cluster,sr = input$table_maps_rows_selected)
      save = try( expr = write.asciigrid(outgrid,fname) , TRUE)
      if(class(save)=="try-error")
        showNotification(ui = "Ошибка при сохранении файла",
                         type = "error")      
    }
  )
  #CB: GMM model ####    
  recalcGMM <- reactive ({
    if(input$numClassUD) nClasses = 0
    else nClasses = input$numClasses
    modDial = modalDialog( "Кластеризация Gaussian Mixture - EM...",
                           title = "Ожидайте...", footer = modalButton("Закрыть"))
    showModal(modDial)
    myReactives$gmm <- calcGMM(myReactives$liveMaps,nClasses)
    #myReactives$gmm <- calcGMM(myReactives$liveMaps,nClasses)
    removeModal() 
    if(input$numClassUD)
      updateSliderInput(session,"numClasses",value = myReactives$gmm$G)
    return(myReactives$gmm)
  })

  #CB: GMM plot model ####
  output$gmmPlot <- renderPlot({
    recalcGMM()
    drawModel(myReactives$liveMaps,myReactives$gmm)
  })

  output$gmmText <- renderText({ #renderPrint renderText
    paste0(capture.output(summary(myReactives$gmm)),"\n")
  })

  output$gmmXText <- renderText({ #renderPrint renderText
    getModelMapText(myReactives$gmm_map)
  })
  
  output$gmmXPlot <- renderPlot({
    recalcGMM()
    myReactives$gmm_map = drawModMapPlot(myReactives$liveMaps,
                                         myReactives$gmm,
                                         sr = input$table_maps_rows_selected)
    par(new = TRUE)
    drawWells(wells = myReactives$wells)
  })
  
  output$gmAuxPlot <- renderPlot({
    recalcGMM()
    drawModBIC(model = myReactives$gmm,mode = input$gmmAuxMode)
  })
  
  #CB: GMM download  ####
  output$downloadGMMap <- downloadHandler(
    filename = function() {
      paste0('GMM_Ncls',input$numClasses,'_',Sys.Date(), '.asc')
    },
    contentType = '.asc',
    content = function (fname) {
      outgrid = saveModMap(myReactives$liveMaps,
                           myReactives$gmm$classification,
                           sr = input$table_maps_rows_selected)
      save = try( expr = write.asciigrid(outgrid,fname) , TRUE)
      if(class(save)=="try-error")
        showNotification(ui = "Ошибка при сохранении файла",
                         type = "error")      
    }
  )

  #CB: HC model ####    
  recalcHCM <- reactive ({
    modDial = modalDialog( "Иерархическая кластеризация...",
                           title = "Ожидайте...", footer = modalButton("Закрыть"))
    showModal(modDial)
    myReactives$hcm <- calcHC(myReactives$liveMaps,
                              mode = hcmModes_hcluster[[input$hcMode]],
                              distMode = hcmDistModes[[input$hcDistMode]])
    removeModal() 
    return(myReactives$hcm)
   # if(input$numClassUD)
    #  updateSliderInput(session,"numClasses",value = myReactives$gmm$G)
  })
  
  #CB: HC plot model ####
  output$hcPlot <- renderPlot({
    recalcHCM()
    drawModel(myReactives$liveMaps,myReactives$hcm)
  })
  
  output$hcText <- renderText({ #renderPrint renderText
    paste0(capture.output(summary(myReactives$hcm_map)),"\n")
  })
  
  output$hcXPlot <- renderPlot({
    recalcHCM()
    myReactives$hcm_map = drawModMapPlot(myReactives$liveMaps,
                                         myReactives$hcm,
                                         sr = input$table_maps_rows_selected,
                                         nClass = input$numClasses)
    par(new = TRUE)
    drawWells(wells = myReactives$wells)
  })
  
  output$hcXText <- renderText({ #renderPrint renderText
    getModelMapText(myReactives$hcm_map)
    
  })

  output$hcAuxPlot <- renderPlot({
    recalcHCM()
    drawModBIC(model = myReactives$hcm,mode = input$hcmAuxMode)
  })
  
  #CB: HC download  ####
  output$downloadHCMap <- downloadHandler(
    filename = function() {
      paste0('HCM_Ncls',input$numClasses,'_',Sys.Date(), '.asc')
    },
    contentType = '.asc',
    content = function (fname) {
      outgrid = saveModMap(myReactives$liveMaps,
                           myReactives$hcm$classification,
                           sr = input$table_maps_rows_selected)
      save = try( expr = write.asciigrid(outgrid,fname) , TRUE)
      if(class(save)=="try-error")
        showNotification(ui = "Ошибка при сохранении файла",
                         type = "error")      
    }
  )
  
  # CB: Zoom maps on Click ####
  zoomPlotCall <- function (name, data) {
    modalDialog( name,size = "l",
                 tagList(plotOutput("zoomPlot", 
                                    dblclick = "plot_zoom_close",
                                    brush = "plot_zoom_brush",
                                    hover = "plot_zoom_hover",
                                    #width = "900px",
                                    height = "800px")
                  ),
                 footer = tagList(
                   actionButton("plot_zoom_xyz","XYZ"),
                   actionButton("plot_zoom_reset","Сбросить увеличение"),
                   modalButton("Закрыть")
                   )
    )
  }
  # CB: Zoom maps Text ####
  #output$plot_zoom_xyz <- renderText({
  observeEvent(input$plot_zoom_hover, {
    xyz = NA
    xy = data.frame(x = input$plot_zoom_hover$x,y = input$plot_zoom_hover$y)
#    browser()
    labs = c(" X:","Y:","Z:")
    if(length(xy) == 0) {
      lab=paste0(labs,xy)
      updateActionButton(session,"plot_zoom_xyz",label = lab)
    }
    else 
    {
      coordinates(xy) = ~x+y
      if(input$maps =='modelKM' && input$modelKM == 'res') 
        xy$z = extract(myReactives$km_map,xy)
      else if (input$maps =='modelGM' && input$modelGM == 'res')
        xy$z = extract(myReactives$gmm_map,xy)
      else if (input$maps =='modelHC' && input$modelHC == 'res')
        xy$z = extract(myReactives$hcm_map,xy)
      #browser()
      lab=paste0('[',prettyNum(xy@coords[1]),':',prettyNum(xy@coords[2]),'] = ',prettyNum(xy$z))
      updateActionButton(session,"plot_zoom_xyz",label = lab)
    }
  })
  # CB: Zoom maps Zoom ####
  observeEvent(input$plot_zoom_reset, {
    myReactives$plot_zoom <- NULL
    session$resetBrush("plot_zoom_brush")
  })
  
  observeEvent(input$plot_zoom_brush , {
    myReactives$plot_zoom <- rbind(c(input$plot_zoom_brush$xmin,input$plot_zoom_brush$xmax),
                              c(input$plot_zoom_brush$ymin,input$plot_zoom_brush$ymax))
  })
  
  observeEvent(input$plot_zoom_close, {
    removeModal() 
  })
  
  observeEvent(input$plot_zoom, {
    showModal(zoomPlotCall(""))
  })
  
  output$zoomPlot <- renderPlot({
    if(input$maps == 'modelKM') {
      #recalcGMM()
      if(input$modelKM == 'mod')
        drawModel(myReactives$liveMaps,myReactives$km)
      else if (input$modelKM == 'res'){
        drawModMapPlot(myReactives$liveMaps,
                       myReactives$km,
                       sr = input$table_maps_rows_selected,
                       zoom = myReactives$plot_zoom)
        par(new = TRUE)
        drawWells(wells = myReactives$wells)
      }
    } else if (input$maps == 'modelGM') {
      if(input$modelGM == 'mod') 
        drawModel(myReactives$liveMaps,myReactives$gmm)
      else if (input$modelGM == 'bic'){
          drawModBIC(myReactives$gmm)
        } else {
        drawModMapPlot(myReactives$liveMaps,
                       myReactives$gmm,
                       sr = input$table_maps_rows_selected,
                       zoom = myReactives$plot_zoom)
        par(new = TRUE)
        drawWells(wells = myReactives$wells)
      }
    } else if (input$maps == 'modelHC') {
      if(input$modelHC == 'mod') 
        drawModel(myReactives$liveMaps,myReactives$hcm)
      else if (input$modelHC == 'res'){
        drawModMapPlot(myReactives$liveMaps,
                       myReactives$hcm,
                       sr = input$table_maps_rows_selected,
                       zoom = myReactives$plot_zoom,
                       nClass = input$numClasses)
        par(new = TRUE)
        drawWells(wells = myReactives$wells)
      }
    }
  })
  
  #CB: plot simple xplot ####
  output$xPlot_simple <- renderPlot({
    map_idx1 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap1)
    map_idx2 = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap2)
    drawXYplot(getXYvectors(myReactives$maps[[map_idx1]], myReactives$maps[[map_idx2]]),input$transp)
  })
  
  #CB: draw wells Table ####
  output$table_wells <- renderDataTable({
    drawWellsTable(myReactives$wells)
  })
  
  #CB: draw maps Table ####
  output$table_maps <- renderDataTable({
    drawMapsTable(myReactives$maps,input$table_maps_rows_selected)
  })

  #CB: set Tabs names ####
  output$tab1 = renderText(({
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap1)
    if(is.null(map_idx)) paste('Проверьте выбор карт')
    else basename(myReactives$maps[[map_idx]]$fn)
  }))
  output$tab2 = renderText(({
    map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = input$selectMap2)
    #browser()
    if(is.null(map_idx)) paste('Проверьте выбор карт')
    else basename(myReactives$maps[[map_idx]]$fn)
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
