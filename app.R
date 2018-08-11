#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


### TODO: ####
###   DONE: 1. Control Point loading
###   DONE: 2. Add CP managing
###   DONE: 3. linear models evaluation
###   DONE: 4. Linear models results presentation
###   DONE: 5. Multiple (more than 2) maps loading 
###   DONE: 6. Maps Management
###   INPROGRESS: 7. provide different Prediction types 
####  INPROGRESS: 7.1. classification tools
##### FIXED: 7.1.1 Check the training - the xplot looks strange of the given NNET complexity
####  INPROGRESS: 7.2. Neural networks tools
####  DONE: 7.2.1 Add NNET maps prediction
####  DONE: 7.2.2 Add saving of the prediction result
####  DONE: 7.2.3 Add NNET QC tools
####  DONE: 7.2.4 Add transfer function selection
####  TODO: 7.2. FIX - transfer the Min and Max from the maps to the scaling/normalizing to preserve prediction range.
####  TODO:?7.3. Linear Models types selection 
####  DONE: 7.3.1. Add prediction map dispay with wells
####  DONE: 7.3.2. Group the QC plots as for other methods
####  DONE: 7.4. Gaussian Mixture model clastering
##### DONE: 7.4.1 GMM automatic classes number diable check
##### DONE: 7.4.2 Add BIC plot to GMM
##### DONE: 7.5 Add Hclust to clusterization methods
##### FIXED: 7.5.1 Fix Hclust Model and result Labeling
###   FIXED: WA the Hclust limitation due to memory consumption. (propagate the lmited product)
####  DONE: 7.6 Add (support vector machines)SVM clustering and prediction
####  TODO: 7.7 Add parameters tuning analysis to model/result plots/panels
###   TODO: 8. Add cross-validation analysis (if not included in LM)
###   DONE: 9. Provide batch maps loading (by path and extension)
##    DONE: FIX Well labeling order issue
##    DONE: FIX Maps selection assert for LM
###   DONE: 10. Add results export/output 
####  DONE: 10.1 Maps (download + write.ascigrid() )
####  DONE: 10.2 CP Table with extracted values
###   DONE: 11. How to remove redunant calls to getLiveMaps()
###   DONE: 12. Add modal messages during long-term calculations/loading
####  DONE: 12.1 Add progress idication to the Modal Panels
###   DONE: 13. Move Maps-only classification tabs from Models to Maps Tab
###   DFRD: 14. Apply Upscaling only to selected maps, if any, to reduce processing time. (manage upscaling parameter?)
###   DONE:?15. Transform the Model/Reslut tabs into radio group and reduce the plot+text to one per model type
###   DONE:?16. Add palette selection for the maps
###   DONE:!17. Add maps averaging
###   DONE: 18. Add bilinear smoothing for Rasters DIsplay plots
###   DONE: 19. Add maps selection for the Map display tabs
###   DONE: 20. Add KM and GMM classes id remapping after calculation according to the Wells' CPs
####  DFRD:?20.1 Add classes remapping for classes not covered by wells through the classes covered by wells' CP
###   DONE: 21. Add Results Zoom option modal popup
###   DONE: 23. Add Brush zoom in the Results Modal popups.
###   DONE: 24. Add Hover text with X.Y.Z on the Results zoom modal popup
###   DONE: 26. Added the 0.5 transparency in Classification results plot
###   DFRD:?27. Make use of raster Layers instead of separate rasters
###   DONE: 28. Add input Maps transparency setting
###   DONE: 29. Add optional Contours display for input maps
###   DONE: 30. Tune the hover delay in zoom modal plots
###   TODO: 31. Estimate and print the StD of wells' CP for each class after sortClasses func.
###   DONE: 32. Add Export format Selection (XYZ/ESRI/ZAMP?)
###   DONE: 33. Refine the Histogram display with density plots overlay and maybe some additional statistics (mean,median,std)
###   DONE: 34. Add option to add Classification/prediction results directly to input MapsList
###   FIXME: Maps selection with 1 additional map works oddly (select callback issue)
###   TODO: 35. Make the maps min and max to be used in the training datasets (in normalization)
###   TODO: 36. Automate the averaging influence estimation
###   TODO: 37. Create a batch mode for all tools depending on: averaging, inputs maps cycle, wells subset
###   TODO: 38. Add Supervised classification option
###   TODO: 38.1 option: Add loading of well CP classes (lithology)
###   TODO: 38.2 option: Add option to make the Wells CP a Factor to be used for SVCLS
###   TODO: 39 Add flag (add model copy fieled? ) to the map object to indicate that map is result of local processing and to not apply the upscaling to it in this case - to make it consistent with inputs. (issue: if upscaling chaned, maps hould be recalculated. maybe automatically?)



#### QC regression model
# m = m100
# k = NULL
# qqq = data.frame(cbind(x = m$V1,y = predict(svm(V1~.,data = m ))))
# plot(qqq,main = k)
# abline(lm(y~x,data = qqq))

#### QC unsupervised SVM model
#m = m100
#k = NULL
#qqq = cbind(m,pp = as.numeric(predict(svm(m))))
#plot(qqq,col = c("red","blue")[qqq$pp+1], pch = 16,main = k)


require(shiny)
require(shinyjs)
require(DT)
require(shinythemes)

require(readr)
require(raster)
#require(miniUI)

# NOTE: detach(package)

#library(spatial)
require(sp)
require(RANN)
require(RSNNS)
require(e1071)
require(mclust)
require(amap)
require(hexbin)
require(parallel)
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
nDefWels = 7
wells0 <- as.data.frame(list( paste0("WELL",1:nDefWels),
            runif(nDefWels,min = map0@bbox[1,1],max = map0@bbox[1,2]),
            runif(nDefWels,min = map0@bbox[2,1],max = map0@bbox[2,2]),
            runif(nDefWels,min = min(map0@data,na.rm = T),max = max(map0@data,na.rm = T))))
#browser()
names(wells0) <- c("WELL", "X_LOCATION","Y_LOCATION","Values")
coordinates(wells0) <- ~X_LOCATION+Y_LOCATION
maps0 = list(NULL,NULL) #list(def_map,def_map)
map_zoom = NULL


source("nnet_plot_update.r") ## Acsource from https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r'
## INIT: selectLists ####
#source("app_ui.R")#, echo = F,encoding = "UTF-8" )
flist <- list(mean,median,sd)
names(flist) <- c('Среднее','Медиана','Ст.Откл.')
#names(flist) <- c("Mean","Median","StdDev")

auxList <- list("BIC","density","uncertainty")
names(auxList) <- c("Критерий информативности Байеса", 
                    "Функция плотности вероятности",
                    "Кроссплот с учетом неопределенности")
nnetDispMode <- list("mod","net","xplot","res")
names(nnetDispMode) <- c("Модель","Сеть","Кроссплот","Прогноз")

glmDispMode <- list("mod","xplot","res")
names(glmDispMode) <- c("Модель","Кроссплот","Прогноз")

svmDispMode <- list("mod","xplot","res")
names(svmDispMode) <- c("Модель","Кроссплот","Прогноз")

svmModels <- list("nu-regression",
                  "eps-regression",
                  "C-classification",
                  "nu-classification",
                  "one-classification"
                  )

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

hcmDistModes <- list("pearson","euclidean", "maximum", "manhattan", "canberra", "binary", 
                      "abspearson", "correlation", "abscorrelation", 
                     "spearman" , "kendall")
names(hcmDistModes) <- c("pearson","euclidean", "maximum", "manhattan", "canberra", "binary", 
                          "abspearson", "correlation", "abscorrelation", 
                         "spearman" , "kendall")

mlpActFuns <- list (1,2,3,4)
names(mlpActFuns) <- c("Logistic", "Hyperbolic tangent", "Gauss","Identical function")

mapPalList <- list (terrain.colors, heat.colors, topo.colors, 
                    rainbow,bpy.colors, gray.colors)
names(mapPalList) <- c("Покров", "Тепло", "Рельеф", 
                       "Радуга", "ЧРЖ", "Монохром")

mapFormats <- list (write.asciigrid,write_csv,write_excel_csv,write_tsv)
mapFormatsRead <- list (read.asciigrid,read_csv2,read_csv2,read_tsv)
names(mapFormats) <- c("ESRI ASCII грид",
                       "Формат CSV",
                       "Формат CSV (Excel)",
                       "Разделитель - табуляция")
names(mapFormatsRead) <- c("ESRI ASCII грид",
                       "Формат CSV",
                       "Формат CSV (Excel)",
                       "Разделитель - табуляция")

batModels <- list ('GLM','NNET','SVM')

classRange = 2:16
classPalette = mclust.options("classPlotColors")
mapPalette = terrain.colors

# INIT: width definition ####
map_hei = "900px"
map_wid = "900px" #"900px"
hist_hei = "200px"
hist_wid = "250px"
butt_wid = "200px"
#modPlot_wid = "500px"
modPlot_wid = 500
busy_size = "50px"
spacer_wid = 20

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
  # Application title
  titlePanel("Анализ карт и контрольных точек"),
  shinythemes::themeSelector(),
  # test = enc2utf8(c("привет","пока"))
  
  #UI: Sidebar with a slider input for number of bins ####
  sidebarLayout(
    sidebarPanel(
      tabsetPanel( id = "main",
        #UI: Wells inputs and display ####
        tabPanel(
          "Скважины", value = "wells",
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
          "Карты", value = 'maps',
          # UI: Class definition ####
          conditionalPanel(
            condition = "input.maps == 'modelKM' || input.maps == 'modelGM' || input.maps == 'modelHC'",
            div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/3*2),"px;"),sliderInput(
              "numClasses",
              "Число классов:",
              min = min(classRange),
              max = max(classRange),
              step = 1,
              value = 3
            )),
            div(style=paste0("display: inline-block;vertical-align:top; width: ", spacer_wid,"px;"),HTML("<br>")),
            div(style=paste0("display: inline-block;vertical-align:top; margin-top: ", spacer_wid,"px; width: ", as.integer(modPlot_wid/3),"px;"),checkboxInput(
              "numClassUD",
              "Автоматически",
              FALSE
            ))
          ),
          tabsetPanel( id = "maps",
                       #UI: Maps input  ####
                       tabPanel( "Данные", value = "input",
                                 div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),sliderInput(
                                   "bins",
                                   "Бины гистограммы:",
                                   min = 1,
                                   max = 100,
                                   value = 30,
                                   step = 10
                                 )),
                                 div(style=paste0("display: inline-block;vertical-align:top; width: ", spacer_wid,"px;"),HTML("<br>")),
                                 div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),sliderInput(
                                   "rstr_fact",
                                   "Разрешение грида:",
                                   min = 0.1,
                                   max = 1,
                                   value = 0.5,
                                   step = 0.1
                                 )),
                                 #div(style=paste0("display: inline-block;vertical-align:top; width: ", spacer_wid,"px;"),HTML("<br>")),
                                 div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),sliderInput(
                                   "rstr_focal",
                                   "Окно осреднения:",
                                   min = 0,
                                   max = 5,
                                   value = 0,
                                   step = 0.2
                                 )),
                                 div(style=paste0("display: inline-block;vertical-align:top; width: ", spacer_wid,"px;"),HTML("<br>")),
                                 div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),selectInput("focalMethod", "Метод:"
                                             ,choices = names(flist), width = butt_wid)),
                                 div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),fileInput(
                                   "Mapsfile",
                                   "Открыть/Заменить карту:",
                                   accept = '.asc',#c("text/plain",
                                   #"text/esri-asciigrid,text/plain",
                                   #"*.asc"),
                                   buttonLabel = "Открыть...",
                                   multiple = TRUE
                                 )),
                                 div(style=paste0("display: inline-block;vertical-align:top; width: ", spacer_wid,"px;"),HTML("<br>")),
                                 div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),selectInput("mapFormatSel", "Формат файлов загрузки/выгрузки"
                                             ,choices = names(mapFormats)
                                 )),
                                 dataTableOutput('table_maps'),
                                 actionButton("delmap"   , "Удалить выбранные", width = butt_wid)
                       ),
                       #UI: HexPlot display ####
                       # tabPanel(  value = "xplot",  title = "Гексаплот",
                       #            plotOutput(
                       #              "xPlot_hex",
                       #              click = "plot_zoom",
                       #              dblclick = "plot_dblclick",
                       #              hover = "plot_hover",
                       #              brush = brushOpts( id = "plot_brush", clip = FALSE, resetOnNew = TRUE)
                       #            ),
                       #            sliderInput(
                       #              "cells",
                       #              "размер бина кроссплота:",
                       #              min = 1,
                       #              max = 50,
                       #              value = 30
                       #            )
                       # ),
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
                                                        actionButton('addKMMap2inp', 'Поместить во входные'),
                                                        verbatimTextOutput("kmXText")
                                              )
                                 )
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
                                                         actionButton('addGMMap2inp', 'Поместить во входные'),
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
                                                         actionButton('addHCMap2inp', 'Поместить во входные'),
                                                         verbatimTextOutput("hcXText")
                                               )
                                  )
                                  ),#,                                  verbatimTextOutput("hcXText")
                       #UI: model SVMC ####
                       tabPanel(  "SVM", value = "modelSV",
                                  tabsetPanel( id = "modelSV",
                                               tabPanel( "Модель", value = "mod",
                                                         plotOutput(
                                                           "svPlot",
                                                           click = "plot_zoom",
                                                           height = modPlot_wid
                                                         ),
                                                         verbatimTextOutput("svText")
                                               ),
                                               tabPanel( "Результат", value = "res",
                                                         plotOutput(
                                                           "svXPlot",
                                                           click = "plot_zoom",
                                                           height = modPlot_wid
                                                         ),
                                                         downloadButton('downloadSVmap', 'Сохранить карту'),
                                                         actionButton('addSVMap2inp', 'Поместить во входные'),
                                                         verbatimTextOutput("svXText")
                                               )
                                  )#,                                  verbatimTextOutput("hcXText")
                       )
          )),
        #UI: models ####
        tabPanel(
          "Модели", value = "models",
          tabsetPanel( id = 'models',
            #UI: model NNET ####
            tabPanel(
              "NNET", value = 'nnet',
              verbatimTextOutput("nnetText"),
              radioButtons("nnetAuxMode",
                           label = "",
                           choices = nnetDispMode,
                           inline=T
              ),
              conditionalPanel(
                condition = "input.models == 'nnet' && input.nnetAuxMode == 'mod' || input.models == 'modelHC'",
              div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),sliderInput(
                "nnet_complex",
                "Сложность сети, %:",
                min = 10,
                max = 90,
                value = 10,
                step = 5
              )),
              div(style=paste0("display: inline-block;vertical-align:top; width: ",spacer_wid,"px;"),HTML("<br>")),
              div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),sliderInput(
                "test_ratio",
                "Тестовая выборка, %:",
                min = 10,
                max = 90,
                value = 30,
                step = 5
              )),
              #div(style=paste0("display: inline-block;vertical-align:top; width: ",spacer_wid,"px;"),HTML("<br>")),
              div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),sliderInput(
                "max_iter",
                "Число итераций:",
                min = 10,
                max = 200,
                value = 50,
                step =5
              )),
              div(style=paste0("display: inline-block;vertical-align:top; width: ", spacer_wid,"px;"),HTML("<br>")),
              div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(modPlot_wid/2),"px;"),selectInput("nnetActFunc", "Функция передачи:"
                    ,choices = names(mlpActFuns), width = butt_wid))),
              plotOutput(
                "nnetPlot",
                click = "plot_zoom",
                height = modPlot_wid
              )
            ),
            #UI: model GLM ####
            tabPanel(
              "GLM", value = 'glm',
              verbatimTextOutput("glmText"),
              radioButtons("glmAuxMode",
                           label = "",
                           choices = glmDispMode,
                           inline=T
              ),
              plotOutput(
                "glmPlot",
                click = "plot_zoom",
                height = modPlot_wid
              )
            ),
            #UI: model SVM ####
            tabPanel(
              "SVM", value = 'svm',
              verbatimTextOutput("svmText"),
              radioButtons("svmAuxMode",
                           label = "",
                           choices = svmDispMode,
                           inline=T
              ),
              plotOutput(
                "svmPlot",
                click = "plot_zoom",
                height = modPlot_wid
              ),
              selectInput("svmModelType","Тип модели", 
                          choices = svmModels)
            )
            
          ),
          downloadButton('downloadModMap', 'Сохранить карту'),
          actionButton('addModMap2inp', 'Поместить во входные'),
          verbatimTextOutput("modelsXText")
        ),
        #UI: BATCH ####
        tabPanel(
          "Поиск", value = "batch",
          #tabsetPanel( id = 'batch',
                       radioButtons("batModel",
                                    label = "",
                                    choices = batModels,
                                    inline=T
                       ),
                       plotOutput(
                         "batPlot",
                         click = "CCplot_get_model",
                         height = modPlot_wid
                       ),
                       sliderInput('CClimit','Минимальный коэффициент корреляции на карте',
                                   min = 0.7,max = 0.98,value = 0.7,step = 0.02)
          #)
        )
      )
      
      # Show a plot of the generated distribution
      ,width = 4, fluid = FALSE),
    #), width = 3 ), #
    mainPanel(
      
      tabsetPanel( id = "mapsTabs",
        # UI: Tabs with Maps and related ####
        tabPanel(
          #UI: Tab1 ####
          uiOutput("tab1"), value = "mapTab1",
          flowLayout(
            flowLayout(
              verticalLayout(
                flowLayout(
                  selectInput("selectMap1", "Карта"
                              ,choices = NULL, width = butt_wid 
                  )
                ),
                flowLayout(
                  actionButton("unzoom1"   , "Сброс масштаба  ", width = butt_wid),
                  actionButton("transpose1"    , "Транспонировать ", width = butt_wid),
                  checkboxInput("showContours1","Контуры", width = butt_wid),
                  checkboxInput("interpMap1","Интерполяция", width = butt_wid),
                  selectInput("mapPalSelect1", "Палитра"
                              ,choices = names(mapPalList), width = butt_wid),
                  sliderInput(
                    "mapPalAlpha1",
                    "Прозрачность:",
                    min = 0.0,
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
          uiOutput("tab2"), value = "mapTab2", 
          flowLayout(
            flowLayout(
              verticalLayout(
                flowLayout(
                  selectInput("selectMap2", "Карта"
                              ,choices = NULL, width = butt_wid
                  )
                ),
                flowLayout(
                  actionButton("unzoom2"   , "Сброс масштаба  ", width = butt_wid),
                  actionButton("transpose2"    , "Транспонировать ", width = butt_wid),
                  checkboxInput("showContours2","Контуры", width = butt_wid),
                  checkboxInput("interpMap2","Интерполяция", width = butt_wid),
                  selectInput("mapPalSelect2", "Палитра"
                              ,choices = names(mapPalList), width = butt_wid),
                  sliderInput(
                    "mapPalAlpha2",
                    "Прозрачность:",
                    min = 0.0,
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
upscaleMap <- function(map_obj = NULL, fact = 1.0, func = mean, focalSize = 0) {
  if(is.null(map_obj)) return(NULL)

  rstr = raster(map_obj$map)
  if (fact > 1.0) 
    rstr = disaggregate(rstr,fact = fact, method = 'bilinear')
  else if (fact < 1.0) 
    rstr = aggregate(rstr,fact = ceiling(1.0/fact), expand = TRUE, fun = mean)
  
  if(focalSize>0)
  {
    #browser()
    stepx=(rstr@extent@xmax-rstr@extent@xmin)/rstr@ncols
    stepy=(rstr@extent@ymax-rstr@extent@ymin)/rstr@nrows
    msize = floor(focalSize*1000/max(stepx,stepy)/2)*2+1
    if( msize < rstr@nrows && msize < rstr@ncols && msize > 1 )
    rstr = focal(rstr, w = matrix(1,msize,msize), fun = func, 
                 pad = F, na.rm = T , NAonly = F)
  }
  map_obj$rstr = rstr
  map_obj$mat = as.vector(rstr)
  
  return(map_obj)
}

## load map file in ESRI ascii grid format, with optional transposing of the matrix ##
loadMapFile <- function (file_obj = NULL, transpose = FALSE, loadFunction = "ESRI ASCII грид", spgid = NULL) {
  #cat(capture.output(loadFunc))
  if(is.null(spgid)) {
    loadFunc = mapFormatsRead[[loadFunction]]
    if (is.null(file_obj)) {
      map = try( expr = loadFunc("../Data/Maps/default.asc") , TRUE)
      if(class(map)=="try-error"){
        return(NULL)
      }
      fn = "default.asc"
    } else {
      map = try( expr = loadFunc(file_obj$datapath), TRUE)
      if(class(map)=="try-error"){
        return(NULL)
      }
      fn = file_obj$name
    }
    if(loadFunction != "ESRI ASCII грид") {
      browser()
      rstr = rasterFromXYZ(map)
      map = as(rstr,'SpatialGridDataFrame')
    } else {
      rstr = raster(map)
    }
  } else {
    fn = basename(names(spgid@data))
    map = spgid
    rstr = raster(spgid)
  }
  
  names(map@data) = fn
  ##projection(map) <- "+proj=longlat +datum=WGS84"
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

# Debug information print with call info
dbgmes <- function (message = "message", expr = NULL, depth = 1) {
  #browser()
  calls = sys.calls()
  depth=min(depth,length(calls))
  range = c(length(calls)-depth:length(calls))
  funame = unlist(strsplit(x = as.character(calls[range]),split = "\\("))
  cat(paste0('==>',funame[1],':\n'))
  cat(paste0(message,':',capture.output(expr),'\n'))
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
  if(length(cpdata$X3)>length(wls[closest$nn.idx,cpname])) {
    return(wells)
  }
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
  else map_idx = sr[idx]
  #map_idx = as.integer(map_idx)
  #if(is.na(map_idx)) browser()
  #dbgmes(message = "sr=",sr)
  #dbgmes(message = "out_idx=",c(idx,map_idx))
  return(map_idx)
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


drawRstr <- function (map = def_map$rstr, zoom = NULL,  pal = mapPalette, alpha = 0, contours = F, interpolate = TRUE) {
  colors = pal(128)
  if(!is.null(alpha))
    colors = setPaletteTransp(colors,1-alpha)

  if(!is.null(zoom)) {
    plot(map, xlim = zoom[1,], ylim = zoom[2,],interpolate=interpolate,col = colors)
  } 
  else {
    zoom <- NULL
    par(new = TRUE)
    plot(map,interpolate=interpolate,col = colors)
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
      #browser()
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


drawHist <- function (map, nbins) {
  map_m = map$mat
  x <- map_m[!is.na(map_m)]
  bins <- seq(min(x), max(x), length.out = nbins + 1)
  # draw the histogram with the specified number of bins
  #browser()
  #dbgmes("name=",names(map_m))
  ddd=density(map$rstr)
  par("mar" = c(2.0,2.0,2.0,2.0) )
  hist(
    x,
    breaks = bins,
    col = 'darkgray',
    border = 'darkgray',
    xlab = "",#paste(length(x),"samples"),
    ylab = "",
    main = paste("Гистограмма "),
    plot = T,
    prob=T
  ) 
  lines(ddd$x,ddd$y,lwd = 1)

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



prepDataSet <- function (wells = NULL, rows = NULL, sel_maps = NULL) {
  #browser()
  if(is.null(wells) || length(wells[1,])<4) return(NULL)
  if(is.null(sel_maps) || length(sel_maps)<1)
    data = wells
  else {
    # FIXED: check and fix the 'sel_maps' contents 
    #        by the size of maps array (issue after map deletion)
    #browser()
    nmap = length(myReactives$maps)
    sel_maps = sapply(sel_maps,FUN = function(x) {
      if(x>nmap) return(NA)
      else return(x)
    })    
    sel_maps = sel_maps[!is.na(sel_maps)]
    data = data.frame(wells[,c(1:2,2+sel_maps)])
  }
  #dbgmes("data0=",data)
  #ddd = list()
  #if(!is.null(rows)) {
    #data[rows,] = NA
    #data = data[(-1*rows),]
  #}
  #dbgmes("rows0=",rows)
  for(i in 1:length(data[,1]))
    if(!all(!is.na(data[i,-1])))
      rows = cbind(rows,i)
  #dbgmes("rows1=",-1*rows)
  if(!is.null(rows)) 
    data = data[(-1*rows),]
  
  #browser()
  #dbgmes(message = "ddd=",ddd)
  if(length(data) <1) return (NULL)
  #rownames(data) = ddd$WELL
  return(data)
}

plotError <- function (message = "Error!!!") {
  plot(0,0,t="l");text(0,0,paste("ОШИБКА:",message),col = "red")
}

drawModelQC <- function(fit = NULL){
  if(is.null(fit)) return()
  
  par(mfrow = c(2,2))
  plot(fit)
  return(fit)
}

drawModelXplot <- function(data = NULL, lmfit = NULL, srows = NULL) {
  
  dbgmes(message = "data=",data)
  #data = prepDataSet(data)
  predicted = predict(lmfit, newdata = data)
  #browser()
  measured = data$Values#[data$WELL %in% names(predicted)]
  
  #measured = data$Values[rownames(data) %in% names(predicted)]
  dbgmes(message = "res=",cbind(measured,predicted))
  abl = lm(predicted~measured)
  
  xccf = ccf(measured, predicted, lag.max = 0, plot = F)
  tit = sprintf("Кроссплот CC=%5.2f Rsq=%5.2f", xccf$acf, as.numeric(xccf$acf) ^ 2)
  
  plot(measured, predicted, main = tit, 
       xlim = bbexpand(c(min(measured),max(measured)),0.1),
       ylim = bbexpand(c(min(predicted),max(predicted)),0.1))
  abline(abl)
  
  text(measured,predicted, labels = names(predicted), pos = 1)
  
  
}
getLiveMapsIds <- function (maps = NULL, sr = NULL) {
  if(is.null(maps)) return(NULL)
  nsr = length(sr)
  #browser()
  if(is.null(sr) || nsr<1) {
    sr=c(1:length(maps)) 
  } else {
    for(i in 1:length(sr)) {
      if(sr[i] > length(maps)) {
        sr = sr[-i]
        i=i-1
      }
    }
    if(is.null(sr) || length(sr)<1) {
      sr=c(1:length(maps)) 
    } 
  }
  if(nsr == 1) sr = c(sr,sr)
  #dbgmes(message = "\tsr=",sr)
  return(sr)
}

getLiveMapsData <- function (maps = NULL, sr = NULL) {
  if(is.null(maps)) return(NULL)
  #browser()
  if(is.null(sr) || length(sr)<1) {
    sr=c(1:length(maps)) 
  } else {
    for(i in 1:length(sr)) {
      if(sr[i] > length(maps)) {
        sr = sr[-i]
        i=i-1
      }
    }
    if(is.null(sr) || length(sr)<1) {
      sr=c(1:length(maps)) 
    } 
  }
  #browser()
  #dbgmes("delSel=",sr)
  data = c(1:length(as.vector(maps[[sr[1]]]$rstr@data@values)))
  for(i in 1:length(sr)) {
    if(length(as.vector(maps[[sr[1]]]$rstr@data@values)) != length(as.vector(maps[[sr[i]]]$rstr@data@values)))
      return(NULL)
    if(sr[[i]] <= length(maps)) {
      data = data.frame(data,as.vector(maps[[sr[i]]]$rstr@data@values))
      colnames(data)[i+1] = maps[[sr[i]]]$fn
    }
  }
  #browser()
  for (i in 1:length(data[1,])){
    data = data[!is.na(data[,i]),]
  }
  #browser()
  #dbgmes(message = "upscaling=",c(length(maps),length(data)))
  return(data)
}
saveModMap_old <- function (data = NULL, clusters = NULL, sr = NULL, fname = NULL, format = "ESRI ASCII грид") {
  if(is.null(data) || is.null(data)|| is.null(clusters)) return(NULL)
  # got live cells
  lividx = data[,1]
  if(!is.null(sr) && length(sr)>1) datplot = myReactives$maps[[sr[1]]]$rstr
  else datplot = myReactives$maps[[1]]$rstr
  datplot@data@values[lividx] <- clusters
  datplot_out=datplot
  dxCellsize=(datplot@extent@xmax-datplot@extent@xmin)/datplot@ncols
  dyCellsize=(datplot@extent@ymax-datplot@extent@ymin)/datplot@nrows
  newCellSize = min(dxCellsize,dyCellsize)
  datplot_out@nrows = as.integer(ceiling((datplot@extent@ymax-datplot@extent@ymin)/newCellSize))
  datplot_out@ncols = as.integer(ceiling((datplot@extent@xmax-datplot@extent@xmin)/newCellSize))
  datplot_out = resample(datplot,datplot_out,method = "ngb")
  spgrid = as(datplot_out,'SpatialGridDataFrame') 
  spgrid@grid@cellsize = rep(newCellSize,2)
  #browser()
  if(!is.null(fname))
  {
    wrFunc = mapFormats[[format]]
    if(format == "ESRI ASCII грид") {
      exprWr = as.expression({
        close( file( fname, open="w" ) )
        wrFunc(spgrid,fname)
      })
    } else {
      q= as.data.frame(datplot_out,xy=T)
      exprWr = as.expression({
        close( file( fname, open="w" ) )
        wrFunc(q,fname)
      })
    }
    save = try( expr = exprWr , FALSE)
    if(class(save)=="try-error"){
      showNotification(ui = "Ошибка при сохранении файла",
                       type = "error")      
      cat(save)
    }
  }
  return (spgrid)
}
saveModMap <- function (datplot = NULL, fname = NULL, format = "ESRI ASCII грид") {
  if(is.null(datplot) ) return(NULL)
  # got live cells
  #lividx = data[,1]
  #if(!is.null(sr) && length(sr)>1) datplot = myReactives$maps[[sr[1]]]$rstr
  #else datplot = myReactives$maps[[1]]$rstr
  #datplot@data@values[lividx] <- clusters
  # datplot_out=datplot
  # dxCellsize=(datplot@extent@xmax-datplot@extent@xmin)/datplot@ncols
  # dyCellsize=(datplot@extent@ymax-datplot@extent@ymin)/datplot@nrows
  # newCellSize = min(dxCellsize,dyCellsize)
  # datplot_out@nrows = as.integer(ceiling((datplot@extent@ymax-datplot@extent@ymin)/newCellSize))
  # datplot_out@ncols = as.integer(ceiling((datplot@extent@xmax-datplot@extent@xmin)/newCellSize))
  # datplot_out = resample(datplot,datplot_out,method = "ngb")
  # spgrid = as(datplot_out,'SpatialGridDataFrame') 
  # spgrid@grid@cellsize = rep(newCellSize,2)
  
  spgrid = rstr2spgrid(datplot)
  #browser()
  if(!is.null(fname))
  {
    wrFunc = mapFormats[[format]]
    if(format == "ESRI ASCII грид") {
      exprWr = as.expression({
        close( file( fname, open="w" ) )
        wrFunc(spgrid,fname)
      })
    } else {
      q= as.data.frame(datplot_out,xy=T)
      exprWr = as.expression({
        close( file( fname, open="w" ) )
        wrFunc(q,fname)
      })
    }
    save = try( expr = exprWr , FALSE)
    if(class(save)=="try-error"){
      showNotification(ui = "Ошибка при сохранении файла",
                       type = "error")      
      cat(save)
    }
  }
  return (spgrid)
}

setPaletteTransp <- function(colors = NULL ,alpha = 0.5) {
  if(is.null(colors)) return(NULL)
  
  colors = apply(sapply(colors, col2rgb)/255, 2, 
                 function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 
  #dbgmes("transpal=",colors)
  return(colors)
}

getDataTransp <- function (data = NULL) {
  if(is.null(data))
    dmax=1
  else {
    #dbgmes("data =",data)
    dmax =  0.002/max(density(x=as.matrix(data), n = 128,na.rm=T)$y)
    dbgmes("maxdens =",dmax)
  }
  return(min(1,dmax))
}

getNewMapFromValues <- function (maps=NULL,sr = NULL,values = NULL) {
  if(is.null(maps) || is.null(values)) return(NULL)
  
  
}

rstr2spgrid <- function (datplot = NULL) {
  if(is.null(datplot) ) return(NULL)
  # got live cells
  #lividx = data[,1]
  #if(!is.null(sr) && length(sr)>1) datplot = myReactives$maps[[sr[1]]]$rstr
  #else datplot = myReactives$maps[[1]]$rstr
  #datplot@data@values[lividx] <- clusters
  datplot_out=datplot
  dxCellsize=(datplot@extent@xmax-datplot@extent@xmin)/datplot@ncols
  dyCellsize=(datplot@extent@ymax-datplot@extent@ymin)/datplot@nrows
  newCellSize = min(dxCellsize,dyCellsize)
  datplot_out@nrows = as.integer(ceiling((datplot@extent@ymax-datplot@extent@ymin)/newCellSize))
  datplot_out@ncols = as.integer(ceiling((datplot@extent@xmax-datplot@extent@xmin)/newCellSize))
  datplot_out = resample(datplot,datplot_out,method = "ngb")
  spgrid = as(datplot_out,'SpatialGridDataFrame') 
  spgrid@grid@cellsize = rep(newCellSize,2)
  
  return(spgrid)
}
drawXYplot <- function (xy = NULL, transp = 1) {
  if(is.null(xy)) return()
  xccf <- ccf(xy[[1]], xy[[2]], lag.max = 0, plot = F)
  
  #browser()
  transp = 2 - transp
  mcol = min(1,getDataTransp(data.frame(xy))*transp)
  plot(
    xy[[1]],
    xy[[2]],
    xlab = names(xy)[1],
    ylab = names(xy)[2],
    #xlab = mcol,
    #ylab = dm,
    main = sprintf("Кроссплот CC=%5.2f %s=%5.2f", xccf$acf, parse(text = 'R^2'), as.numeric(xccf$acf) ^ 2),
    col = rgb(0, 0, 1, mcol),#, 1 , maxColorValue = mcol),
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
  
  maps = matrix(,nrow = length(maps_), ncol = 4)
  for( row in 1:length(maps_)) {
    map = maps_[[row]]
    maps[row,] <- c(paste0("Map",row),Name = map$fn, 
                    Min = map$rstr@data@min, Max = map$rstr@data@max)
  }
  #names(maps) <- ' '
  #dbgmes("sel=",sr)
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
      #      selection  = sr,
      target = 'row'
    ),
    options = list(
      pagingType = "simple",
      paging = FALSE,
      ColumnRender = prettyNum,
      scrollY = "400px",
      scrollCollapse = TRUE,
      #      stateSave = TRUE,
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

# PREDICTION ####
#buildNNET <- function(wells = NULL, rows = NULL, sel_maps = NULL, test_ratio = 0.25, max_iter = 100, nnet_complex = 0.1, actFunc=1){
buildNNET <- function(wells = NULL, test_ratio = 0.20, max_iter = 100, nnet_complex = 0.1, actFunc=1){
  if(is.null(wells)) return(NULL)
  #data = prepDataSet(wells = wells@data,rows = rows,sel_maps = sel_maps)
  data = wells
  if(is.null(data)) return(NULL)
  
  size = max(1,ceiling(sqrt(length(data[,1]))*nnet_complex*5*2))
  layers = max(1,ceiling(log(base = 5, size)))
  neurons = ceiling(seq.int(size/2,3, length.out = layers))
  
  
  dset0 = splitForTrainingAndTest(
    x = (data[,c(-1,-2)]),
    y = (data$Values), ratio = 0)
  #nninp = dset0$targetsTrain
  dset0 = normTrainingAndTestSet(dset0,dontNormTargets = FALSE)

  #dbgmes(message = "size,layers,neurons=",c(size,layers,neurons))
  #dbgmes(message = "test_ratio=",test_ratio)
  #browser()
  if(length(dset0$inputsTrain[,1])<4) {
    test_ratio = 0
    dset = dset0
  } else {
    dset = splitForTrainingAndTest(
      x = dset0$inputsTrain,
      y = dset0$targetsTrain, ratio = test_ratio)
  }
  
  #dbgmes(message = "size,layers,neurons=",c(size,layers,neurons))
  #dbgmes(message = "test_ratio=",test_ratio)
  #dbgmes(message = "dset=",dset)
  #browser()
  setACtType <- function (x) {return(actFunc)} 
  if(test_ratio > 0) nnet = mlp(dset$inputsTrain, dset$targetsTrain, 
             size = neurons, 
             learnFuncParams = c(0.5),maxit = max_iter,
             inputsTest = dset$inputsTest, targetsTest = dset$targetsTest,
             linOut = T,actfns=setACtType)
  else nnet = mlp(dset$inputsTrain, dset$targetsTrain, 
                  size = neurons, 
                  learnFuncParams = c(0.5),maxit = max_iter,
                  linOut = T,actfns=setACtType)
  rownames(dset0$inputsTrain) = (data$WELL)
  colnames(dset0$inputsTrain) = colnames(data[,3:length(data[1,])])
  nnout = predict( nnet, newdata = dset0$inputsTrain)  
  nnout = denormalizeData(nnout,getNormParameters(dset0$targetsTrain))
  #dbgmes(message = "res=",cbind(data$Values,nnout[,1]))
  return(list(net = nnet,dset = dset0, out = nnout[,1], inp = data$Values))
}
drawNNETmap <- function (data = NULL ,sr = NULL, nnet = NULL,zoom = NULL) {
  if(is.null(data) || is.null(nnet) || is.null(nnet$net)) return(NULL)
  dset = normalizeData(data[,2:length(data)],getNormParameters(nnet$dset$inputsTrain))
  res = predict(nnet$net,dset)
  res = denormalizeData(res,getNormParameters(nnet$dset$targetsTrain))
  
  #browser()
  lividx = data[,1]
  if(!is.null(sr) && length(sr)>1) 
    datplot = myReactives$maps[[sr[1]]]$rstr
  else
    datplot = myReactives$maps[[1]]$rstr
  title = ""
  names(datplot) = title
  datplot@data@values = NA
  datplot@data@values[lividx] <- res
  datplot@data@values[datplot@data@values==0] <- NA
  
  return (datplot)
}  

drawNNETmodel <-function (nnet = NULL, mode = input$nnetAuxMode) {
  if(mode == "mod")
  {
    par(mfrow=c(2,1))
    #par(new = TRUE)
    
    plot(nnet$net$IterativeTestError,col="red", type = "l",
         xlab = "Iteration",ylab = "Error",
         ylim=c(0,max(nnet$net$IterativeTestError,nnet$net$IterativeFitError)))
    lines(nnet$net$IterativeFitError)
    plot(nnet$net$IterativeFitError,myReactives$nnet$net$IterativeTestError,
         col = heat.colors(length(nnet$net$IterativeTestError))[1:length(nnet$net$IterativeTestError)],
         pch = 16,
         xlab = "Learning Error",ylab = "Test set Error"
    )
    #browser()
    #plotRegressionError(net$net$targetsTrain,nnet$net$fitted.values)
    #plotROC(fitted.values(nnet$net), nnet$dset$targetsTrain)
  } else if(mode == "net") {
    par(new = TRUE)
    par(mfrow=c(1,1))
    plot.nnet(myReactives$nnet$net)
  } else if(mode == "xplot") {
    par(new = TRUE)
    par(mfrow=c(1,1))
    #browser()
    
    measured = nnet$inp
    predicted = nnet$out
    xccf <- ccf(measured,predicted, lag.max = 0, plot = F)
    plot(measured,predicted,
         xlim = bbexpand(c(min(measured),max(measured)),0.1),
         ylim = bbexpand(c(min(predicted),max(predicted)),0.1),
         xlab = "measured",ylab = "predicted",
         main = paste("Корреляция = ", prettyNum(xccf$acf)))
    lmr = lm(formula = predicted~measured)
    #browser()
    abline(lmr)
    text(measured,predicted, 
         labels = rownames(nnet$dset$inputsTrain),
         pos = 1)
    
  }
}

drawGLMmap <- function (data = NULL ,sr = NULL, glm = NULL,zoom = NULL, colors = rainbow(128)) {
  if(is.null(data) || is.null(glm)) return(NULL)

  #dset = data[,2:length(data)]
  dset = data.frame(data[,2:length(data)])
  #dbgmes("dset=",dset)
  #rownames(dset) = 
  if(is.null(sr)) {
    #names(dset) = c(paste0("Map",1:length(dset)))
    colnames(dset) = c(paste0("Map",1:length(dset[1,])))
  } else {
    #names(dset) = c(paste0("Map",sr[1:length(dset)]))
    colnames(dset) = c(paste0("Map",sr[1:length(dset[1,])]))
  }
  #dbgmes("dset=",dset)
  #browser()
  
  res = predict.glm(object = glm,newdata = dset)
  
  lividx = data[,1]
  if(!is.null(sr) && length(sr)>1) 
    datplot = myReactives$maps[[sr[1]]]$rstr
  else
    datplot = myReactives$maps[[1]]$rstr
  title = ""
  names(datplot) = title
  datplot@data@values = NA
  datplot@data@values[lividx] <- res
  datplot@data@values[datplot@data@values==0] <- NA
  
  return (datplot)
}  

buildGLM <- function(data = NULL, lmfunc = glm, family = gaussian){

  if(is.null(data)) return(NULL)
  
  data=data[-1]
  #dbgmes("dset=",data)
  fit = lmfunc(formula = Values~., data, family = family)
  #browser()
  #fit$cvDelta = cv.glm(data = fit$data[!is.na(fit$data)],glmfit = fit,cost = cost,K=7)[3]
  return(fit)
}

drawSVMmap <- function (data = NULL ,sr = NULL, svmod = NULL,zoom = NULL, colors = rainbow(128)) {
  if(is.null(data) || is.null(svmod) || is.null(svmod$mod)) return(NULL)
  
  #dset = data[,2:length(data)]
  dset = data.frame(data[,2:length(data)])
  #rownames(dset) = 
  if(is.null(sr)) {
    #names(dset) = c(paste0("Map",1:length(dset)))
    colnames(dset) = c(paste0("Map",1:length(dset[1,])))
  } else {
    #names(dset) = c(paste0("Map",sr[1:length(dset)]))
    colnames(dset) = c(paste0("Map",sr[1:length(dset[1,])]))
  }
  #dbgmes("dset=",dset)
  #browser()
  
  res = predict(svmod$mod,newdata = dset)
  
  #dbgmes("res=",as.data.frame(res))
  
  lividx = data[,1]
  if(!is.null(sr) && length(sr)>1) 
    datplot = myReactives$maps[[sr[1]]]$rstr
  else
    datplot = myReactives$maps[[1]]$rstr
  title = ""
  names(datplot) = title
  datplot@data@values = NA
  datplot@data@values[lividx] <- res
  datplot@data@values[datplot@data@values==0] <- NA
  
  return (datplot)
}  

drawSVMmap2 <- function (data = NULL ,sr = NULL, svmod = NULL,zoom = NULL) {
  if(is.null(data) || is.null(svmod) || is.null(svmod$mod)) return(NULL)
  dset =data[,2:length(data)]
  res = predict(svmod$mod,dset)
  #res = denormalizeData(res,getNormParameters(nnet$dset$targetsTrain))
  
  #browser()
  lividx = data[,1]
  if(!is.null(sr) && length(sr)>1) 
    datplot = myReactives$maps[[sr[1]]]$rstr
  else
    datplot = myReactives$maps[[1]]$rstr
  title = ""
  names(datplot) = title
  datplot@data@values = NA
  datplot@data@values[lividx] <- res
  datplot@data@values[datplot@data@values==0] <- NA
  
  return (datplot)
}  

buildSVM <- function(data = NULL, test_ratio = 0.25, type = svmModels[1]) {
#  if(is.null(wells)) return(NULL)
#  data = wells #prepDataSet(wells = wells@data,rows = rows,sel_maps = sel_maps)
  if(is.null(data)) return(NULL)
  svm_tune=NULL
  #dbgmes("type=", type)
  #browser()
  #dset = splitForTrainingAndTest(
  #  x = data[,3:length(data[1,])],
  #  y = as.matrix(data$Values), ratio = 0)
  #rownames(data) = (data$WELL)
  #colnames(dset$inputsTrain) = colnames(data[,3:length(data[1,])])
  #browser()
  dset = data[,2:length(data[1,])]
  if(substr(type,nchar(type)-9,nchar(type)-4) != "regres") {
    dset$Values = as.integer(dset$Values)
    class = 1
  } else class = 0
  #browser()
  #dbgmes("dset=", length(dset[,1]))
  if(length(dset[,1]) < 10 || class == 1) {
    svmod = svm(Values~., data = dset, 
                type = type,
                decision.values = TRUE,
                probability = TRUE)
  } else {
    svm_tune =  tune(svm, Values~. , data = dset,
                    ranges = list(epsilon = seq(0,1,0.1)
                                  ,cost = seq(1,5,0.5)
                                  #,cost = 3^(-3:8) 
                                 #,gamma = seq(0,1,0.02)
                                  )
                    #coef0 = 0.5
                    #cost = 4096, kernel = "polynomial", degree = 4,
                    #tunecontrol = tune.control(sampling = "fix"
                    )
    svmod = svm_tune$best.model
  }
  #dbgmes("svmod=", svmod)
   # FROM INET:
   # svmod = svm(abc$a,abc$b)
   # abcres = data.frame(cbind(abc$a,predict(svmod,abc$a)))
   # lm(data = abcres,formula = b~a)
   # plot(abcres)
   # abline(lm(data = abcres,formula = b~a))
   # svm_tune <- tune(svm, a ~ b , data = abc,
   #                  ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
   # )
  return(list(mod=svmod,tune=svm_tune, class = class))
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
  #dbgmes("data=",data)
  #sel = c( 1:length(data$WELL))
  #sel[srows] = NA
  #sel = sel[!is.na(data$Values)]
  #sel = sel[!is.na(sel)]
  predicted = predict.lm(lmfit)
  #measured = data$Values[sel]
  measured = data$Values[data$WELL %in% names(predicted)]
  
  #dbgmes("xplot data = ", c((predicted),(measured)))
  #dbgmes("length = ", c(length(predicted),length(measured)))
  abl = lm(predicted~measured)
  res = paste(abl$call$formula[2]," = ", abl$call$formula[3]," * ",
              prettyNum(abl$coefficients[2]),"+",
              prettyNum(abl$coefficients[1]))
  return(res)
  #plot(anova(myReactives$glm))
}


# CLASSIFICATION ####

drawModel <- function(data = NULL,model = NULL, nclass = 3) {
  #browser()
  #dbgmes(message = "model=",model)
  if(is.null(model)) {
    plotError("Модель не рассчитана.\n проверьте входные данные")
  }
  #browser()
  if(is.null(data)) {
    plotError("Объединенная выборка пуста.\n проверьте входные данные")
  }
  pal = setPaletteTransp(classPalette,getDataTransp(data[-1]))
  
  if(class(model) == 'Mclust') {
    #mclust.options(classPlotColors = pal) 
    plot(model, pch = 16, col = pal,
         what = "classification")
  }
  else if(class(model) == 'kmeans') {
    plot(as.data.frame(data[-1]),#data[,2:length(data[1,])]),
         #main = (capture.output(model))[1],
         col = pal[model$cluster], pch = 16)
    #if(class(model) == 'kmeans')
    #  points(model$centers,pch = 4, cex = 4, lwd = 4)
  }
  else if(class(model) == 'hclust') {
    #plot(model,col = pal)
    #browser()
    #predicted = cutree(model,min(nclass,length(model$height)))
    if(is.null(model$ids))
      dset = data[-1]
    else 
      dset = data[model$ids,-1]
    predicted = cutree(model,min(nclass,length(model$height)))
    model$predicted = predicted
    plot(dset,#data[,2:length(data[1,])]),
         #main = (capture.output(model))[1],
         col = pal[predicted], pch = 16)
  } else if(class(model) == 'svm') {
    plot(as.data.frame(data[-1]),#data[,2:length(data[1,])]),
         #main = (capture.output(model))[1],
         col = pal[model$predicted], pch = 16)
  }
}
drawModMap <- function (datplot = NULL, title = NULL , zoom = NULL, colors = mapPalette(128), interpolate = T) {
  if(is.null(data)) return(NULL)
  #browser()
  #dbgmes("plot=",datplot,depth = 2)
  par(new = TRUE)
  par(mfrow=c(1,1))
  if(is.null(zoom))
    plot(datplot,
         main = title,
         col = colors ,interpolate=interpolate)
  else
    plot(datplot,
         main = title,
         xlim = zoom[1,], ylim = zoom[2,],
         col = colors,interpolate=interpolate)
}

drawModMapPlot <- function (data = NULL, model = NULL, zoom = NULL, sr=NULL, nClass = 3, doPlot = TRUE) {
  if(is.null(data) || is.null(data)|| is.null(model)) return(NULL)
  msize = 1
  #browser()
  if(class(model) == 'Mclust') {
    title = (capture.output(summary(model)))[2]
    clusters = model$classification
  } else if(class(model) == 'kmeans') {
    title = (capture.output(model))[1]
    clusters = model$cluster
  } else if(class(model) == 'svm') {
    #browser()
    title = (capture.output(model))[1]
    #clusters = as.integer(predict(model$mod)) +1
    clusters = model$predicted
  } else if(class(model) == 'hclust') {
    title = (capture.output(model))[5:6]
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
  datplot@data@min = min(datplot@data@values,na.rm = T)
  datplot@data@max = max(datplot@data@values,na.rm = T)
  # focal processing  ####
  if(msize!=1)
    datplot <- focal(datplot, w = matrix(1,msize,msize), fun = fill.na, 
                     pad = TRUE, na.rm = FALSE , NAonly = T)
  datplot = sortClasses(datplot,myReactives$wells)
  
  if(doPlot) {
    colors = classPalette[1:max(clusters)]
    #browser()
    colors = setPaletteTransp(colors,0.5)
    
    drawModMap(datplot = datplot,title = title,
               zoom = zoom,
               colors = colors, interpolate = F)
  }
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

calcKMeans <- function(data = NULL, nclass = 3) {
  if(is.null(data)) return(NULL)
  
  #data = getLiveMapsData(maps,sr)
  kmns = kmeans(scale(as.matrix(data[,2:length(data[1,])])),nclass)
  #kmns = kmeans(data[,2:length(data[1,])],nclass)
  clr=kmns$cluster
  #plot(datplot,col = classPalette("classPlotColors")[1:max(clusters)])
  return(kmns)
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



calcGMM <- function(data = NULL, nclass = 3) {
  if(is.null(data)) return(NULL)
  itmax = 50

  dat = data[-1]#scale(data[-1])
  #names(dat) = names(data[,2:length(data[1,])])
  if(nclass > 0) {
    gmmBIC = mclustBIC(dat,G=nclass, control = emControl(itmax = itmax))
    gmm <- Mclust(dat,x = gmmBIC,G=nclass)
  } else {
    gmmBIC=mclustBIC(dat,G=classRange, control = emControl(itmax = itmax))
    gmm <- Mclust(dat,x = gmmBIC,scale = TRUE)
  }
  
  return(gmm)
}

calcHC <- function(data = NULL, nclass = 3, mode = "complete", distMode = "euclidean", maxPairs = 16000) {
  if(is.null(data)) return(NULL)
  #browser()
  dat = scale(data) #data[,2:length(data[1,])]
  #names(dat) = names(data[,2:length(data[1,])])
  # usage of hclust require dist matrix which size would exceed the available memory
  # in most cases hcluster requires less memory, 
  # but still requires the data decimation by reduceFactor.
  #reduceFactor = as.integer(max(1,length(dat[,1])/65000L^(1/(length(dat[1,])-1))))
  reduceFactor = ceiling(length(dat[,1])/maxPairs)
  if(reduceFactor > 1 ) {
    ids = seq(1,length(dat[,1]),reduceFactor)
    dat= dat[ids,]
    ids = ids
  } else {
    ids = NULL
  }
  hcm = hcluster(dat,method = distMode, link = mode)
  hcm$reduceFactor = reduceFactor
  hcm$ids=ids
#  hcm$used = dat
  #return(list(mod = hcm, predicted = predicted))
  return(hcm)
}

calcSVMC <- function(data = NULL, nclass = 3, type = svmModels[3]) {
  if(is.null(data)) return(NULL)
  #browser()
  svcm = svm(data[-1])
  svcm$predicted = NULL
  return(svcm)
}

# BATCH ####

getCCvalues <- function(data = NULL,modType = NULL,
                        test_ratio = 0.2,
                        nnet_complex = 0.1,
                        max_iter = 50,
                        svmType = svmModels[1],
                        NNactFunc = mlpActFuns[[1]]) {
  
  if(is.null(data)) return(NULL)  
  
  if(modType == "NNET"){
    #dbgmes("bdset=",data)
    nnet = buildNNET(data,
                     test_ratio = test_ratio,
                     max_iter = max_iter,
                     nnet_complex = nnet_complex/100.,
                     actFunc = NNactFunc)
    measured = nnet$inp
    predicted = nnet$out
  } else if (modType == "GLM"){
    glm = buildGLM(data)
    predicted = predict(glm)#, newdata = data)
    measured = data$Values#[data$WELL %in% names(predicted)]
  } else if (modType == "SVM"){
    svm = buildSVM(data,
                   test_ratio = test_ratio,
                   type = svmType)
    predicted = predict(svm$mod, newdata = data)
    measured = data$Values#[data$WELL %in% names(predicted)]
  }
  cc = as.numeric(ccf(measured,predicted, lag.max = 0, plot = F)$acf)
  return(cc)
}

getModelCCmatrix <- function (wells = NULL,minWells = 3,
                              modType = NULL,
                              test_ratio = 0.2,
                              nnet_complex = 0.1,
                              max_iter = 50,
                              svmType = svmModels[1],
                              NNactFunc = mlpActFuns[[1]]) {
  if(is.null(wells) || length(wells[,1])<2) return(NULL)  
  test_ratio=test_ratio/100.
  minWells = minWells - 1
  dbgmes("wells=",wells)
  nw = length(wells[,1])
  nm = length(wells)-2
  dbgmes("nw,nm=",c(nw,nm))

  sel_maps_ = list()
  nmst = 0
  for(im in 1:nm) {
    sel_maps = combn(1:nm,im)
    #dbgmes("sel_maps=",cbind(im,sel_maps))
    #browser()
    nsm = length(sel_maps[1,])
    for(ism in 1:nsm) {
      sel_maps_ = append(sel_maps_,list(sel_maps[,ism]))
    }
  }
  sel_wells_ = list()
  for(iw in 1:(nw-minWells)) {
    sel_wells = combn(1:nw,iw)
    #dbgmes("sel_wells=",sel_wells)
    nsw = length(sel_wells[1,])
    for(isw in 1:nsw) {
      sel_wells_ = append(sel_wells_,list(sel_wells[,isw]))
      #nmst = nmst + 1
    }
  }
  nsm_ = length(sel_maps_)
  nsw_ = length(sel_wells_)
  nmst_ = nsw_*nsm_
  dbgmes("nsm_,nsw_,nmst_=",c(nsm_,nsw_,nmst_))
  
  cc_matrix = matrix(nrow = nsm_,ncol = nw,data = NA)
  dset_matix = array(list(),c(nsm_,nw))

  withProgress(message = "Обработка...", detail = "Ожидайте...",  value =0, max = nmst_, {
  for(ism_ in 1:nsm_) {
    #sel_maps = combn(1:nm,im)
    sel_maps=sel_maps_[ism_][[1]]
    #browser()
    nsm = length(sel_maps)
    dbgmes("sel_maps =",sel_maps)
    for(isw_ in 1:nsw_) {
        sel_wells = sel_wells_[isw_][[1]]
        iw = length(sel_wells)
        addDset = prepDataSet(wells = wells, rows = sel_wells, sel_maps = sel_maps)
        #browser()
          
        cc = getCCvalues(data = addDset,modType = modType,test_ratio = test_ratio,
                                           nnet_complex = nnet_complex,max_iter = max_iter,
                                           svmType = svmType,NNactFunc = NNactFunc)
        if(is.null(cc) || is.nan(cc)) {
          dbgmes("cc=NULL:",addDset)
          cc=0
        }
        #browser()
        c=(nw-iw+1)#(nw-iw+1)
        r=ism_
        incProgress(amount = 1)
        if(is.null(cc_matrix[r,c]) || is.na(cc_matrix[r,c]) || cc>cc_matrix[r,c])
        { 
          cc_matrix[r,c] = cc
          dset_matix[[r,c]] = list(addDset)
        }
      }
      #dbgmes("best_isw_DS=",isw_max_dset)
      #dbgmes("best_isw_CC=",isw_max_cc)
    }
    #dbgmes("best_ism_DS=",ism_max_dset)
    #dbgmes("best_ism_CC=",ism_max_cc)
    #dbgmes("iw,im,cc =",c(iw,im,isw_max_cc))
    
  dbgmes("cc_matrix =",cc_matrix)
  })
  #browser()
  #return(NULL)
  return(list(ccMatrix=cc_matrix,ccDset = dset_matix))
}


getModelCCmatrix_par <- function (wells = NULL,minWells = 3,
                              modType = NULL,
                              test_ratio = 0.2,
                              nnet_complex = 0.1,
                              max_iter = 50,
                              svmType = svmModels[1],
                              NNactFunc = mlpActFuns[[1]]) {
  if(is.null(wells) || length(wells[,1])<2) return(NULL)  
  test_ratio=test_ratio/100.
  minWells = minWells - 1
  dbgmes("wells=",wells)
  nw = length(wells[,1])
  nm = length(wells)-2
  dbgmes("nw,nm=",c(nw,nm))
  
  sel_maps_all = list()
  nmst = 0
  for(im in 1:nm) {
    sel_maps = combn(1:nm,im)
    #dbgmes("sel_maps=",cbind(im,sel_maps))
    #browser()
    nsm = length(sel_maps[1,])
    for(ism in 1:nsm) {
      sel_maps_all = append(sel_maps_all,list(sel_maps[,ism]))
    }
  }
  sel_wells_all = list()
  for(iw in 1:(nw-minWells)) {
    sel_wells = combn(1:nw,iw)
    #dbgmes("sel_wells=",sel_wells)
    nsw = length(sel_wells[1,])
    for(isw in 1:nsw) {
      sel_wells_all = append(sel_wells_all,list(sel_wells[,isw]))
      #nmst = nmst + 1
    }
  }
  nsm_all = length(sel_maps_all)
  nsw_all = length(sel_wells_all)
  nmst_ = nsw_all*nsm_all
  dbgmes("nsm_,nsw_,nmst_=",c(nsm_all,nsw_all,nmst_))
  
  runIdx = apply(matrix(rep(1:nsw_all),nrow=nsw_all, ncol=nsm_all), 1, FUN=paste0, '.', c(1:nsm_all))
  #dbgmes("runIdx:",runIdx)
  
  cc_matrix = matrix(nrow = nsm_all,ncol = nw,data = NA)
  dset_matix = array(list(),c(nsm_all,nw))
  #browser()
  
  applyGetCC <- function (x) {
    q=unlist(strsplit(x,'\\.')); 
    isw_=as.integer(q[1])
    ism_=as.integer(q[2])
    #dbgmes("isw_,ism_:",c(isw_,ism_))
    sel_maps=sel_maps_all[ism_][[1]]
    sel_wells = sel_wells_all[isw_][[1]]
    iw = length(sel_wells)
    #browser()
    #shiny::isolate({
      addDset = prepDataSet(wells = wells, rows = sel_wells, sel_maps = sel_maps)
      
      cc = getCCvalues(data = addDset,modType = modType,test_ratio = test_ratio,
                       nnet_complex = nnet_complex,max_iter = max_iter,
                       svmType = svmType,NNactFunc = NNactFunc)
      if(is.null(cc) || is.nan(cc)) {
        #dbgmes("cc=NULL:",addDset)
        cc=0
      }
      #browser()
      c=(nw-iw+1)#(nw-iw+1)
      r=ism_
      #dbgmes("incProgress")
      shiny::incProgress(amount = 1)
      if(is.null(cc_matrix[r,c]) || is.na(cc_matrix[r,c]) || cc>cc_matrix[r,c])
      { 
        #FIXME: The CC value in result map do not corespond with the dataset somehow
        #dbgmes("set CC & maT",cc)
        cc_matrix[r,c] <<- cc
        dset_matix[[r,c]] <<- list(addDset)
      }
    #})
    #dbgmes("cc=NULL:",addDset)
    return(x)
  }
  
  #browser()
  # myEnv = new.env(parent = globalenv())
  # myEnv$cc_matrix = cc_matrix
  # myEnv$dset_matix = dset_matix
  # myEnv$nw
  withProgress(message = "Обработка...", detail = "Ожидайте...",  value =0, max = nmst_, {
  #isolate({
    #no_cores = detectCores() - 1
    #cl = makeCluster(no_cores)
    # clusterExport(cl=cl, 
    #               varlist=c("cc_matrix", "dset_matix","wells","modType","test_ratio",
    #                                "nnet_complex","max_iter","NNactFunc",
    #                                "svmType","nw","nm",
    #                                "sel_maps_all","sel_wells_all"), envir=environment())
    # parApply(cl = cl,X = runIdx,MARGIN = c(1,2),FUN = applyGetCC)
    apply(runIdx,c(1,2),FUN = applyGetCC)
    #browser()
    #head(myEnv$cc_matrix)
  })
    #mapply(runIdx,FUN = applyGetCC)
    
  #})
  dbgmes("cc_matrix =",cc_matrix)
  #browser()
  #return(NULL)
  #stopCluster(cl)
  return(list(ccMatrix=cc_matrix,ccDset = dset_matix))
}
drawModelCCplot <- function(wells = NULL, CCmod = NULL,CClimit = 0.9) {
  #text(c(1,1),"text")
  if(is.null(wells) || is.null(CCmod) || is.null(CCmod$ccMatrix)) {
    plotError(message = "Не удалось выполнить расчет. Проверьте данные")
    return(NULL)
  } 
  #nw = length(wells[,1])
  #nm = length(wells)-2
  nm = dim(CCmod$ccMatrix)[1]
  nw = dim(CCmod$ccMatrix)[2]
  #browser()
  CCmod$ccMatrix[CCmod$ccMatrix < CClimit] = NA
  rstr=raster(CCmod$ccMatrix[,c(-1:-2)],xmn=1,xmx=nw-2,ymn=1,ymx=nm)
  ylabs = list()
  maxNm = 0
  for(i in 1:nm) {
    names = names(CCmod$ccDset[[i,3]][[1]][-1:-2])
    ylabs = append(ylabs,list(paste0(names,collapse='\n')))
    maxNm = max(maxNm,length(names))
  }
  #dbgmes("ylabs=",ylabs)
  dbgmes("maxNm=",maxNm)
  dbgmes(capture.output(rstr))
  mar = par('mar')
  mgp = par('mgp')
  mar[2] = maxNm+2.1
  mgp[1] = mar[2]-1
  par(mar = mar,mgp = mgp)
  plot(rstr,yaxt="n",xaxt="n",xlim=c(1,nw-2),ylim=c(1,nm),
       xlab="",
       ylab="Карты в выборке",
       main = "Коэффициент корреляции для набора моделей",
       col=bpy.colors(16))
  grid()
  mtext("Скважин в выборке",side=1,line=2)
  xlabs = ""
  #browser()
  #xaxis
  axis(side = 1,at = c(1:(nw-2)), labels = paste(c(3:nw)))#, labels = rownames(wells))
  #yaxis
  axis(side = 2,at = c(1:nm), labels = ylabs)
}


# Define server logic required to draw a histogram
options(shiny.maxRequestSize = 500 * 1024 ^ 2)
#options(shiny.reactlog = TRUE)
options(shiny.host = "0.0.0.0")
options(shiny.port = 8080)
#options(shiny.style="old")
myReactives <- reactiveValues(wells = wells0, 
                              zoom = map_zoom, 
                              fit = NULL, 
                              maps = NULL)


server <- function(input, output, session) {
  # if(is.null(myReactives$maps)) {
  #   myReactives$maps <- list(def_map,def_map)
  # }
    showModDial <- function(message = "Ожидайте...") {
    showModal(modalDialog( list(imageOutput("cycle", 
                                            width = busy_size,
                                            height = busy_size,
                                            inline = T),
                                message),
                           title = "Ожидайте...", footer = modalButton("Закрыть")))
  }
  
  dtMapsProxy = dataTableProxy("table_maps")  
  #CB: Busy Modal diaplay ####
  output$cycle <- renderImage ({
    return(list(
      src = "images/busy.gif",
      contentType = "image/gif",
      alt = "Busy")
      #,style="display: block; margin-left: auto; margin-right: auto;"
      )
  }, deleteFile = FALSE)
  
  #CB: set Zoom rect ####
  observeEvent(input$plot_brush , {
    myReactives$zoom <- rbind(c(input$plot_brush$xmin,input$plot_brush$xmax),
                              c(input$plot_brush$ymin,input$plot_brush$ymax))
  })
  
  recalcMaps <- reactive ({
    #return()
    invalidateLater(5000, session)
    if(is.null(myReactives$maps) || length(myReactives$maps) == 0 ) {
      myReactives$maps <- append(myReactives$maps,list(def_map))
      myReactives$maps <- append(myReactives$maps,list(def_map))
      myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
      myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
      updateMapLists(myReactives$maps,input$table_maps_rows_selected)
    }
    showModDial("Обработка карт...")
    mapsSelection =  input$table_maps_rows_selected 
    withProgress(message = "Обработка карт...", detail = "Ожидайте...",  value =0, {
      for (i in 1:length(myReactives$maps)) {
        map_obj = upscaleMap(myReactives$maps[[i]],input$rstr_fact,func = flist[[input$focalMethod]],focalSize = input$rstr_focal)
        incProgress(detail = paste0('"',map_obj$fn,'"'), amount = 1./length(myReactives$maps))
        myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",i))
        myReactives$maps[[i]] <- map_obj
      }
    })
    myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
    myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
    #myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
    #updateMapLists(myReactives$maps,input$table_maps_rows_selected)
    selectRows(dtMapsProxy,as.numeric(mapsSelection))
    #dbgmes(message = "upscaling=",c(length(myReactives$liveMaps),length(myReactives$maps)))
    removeModal()
  })

  #CB: set focal ####
  observeEvent(input$focalMethod, {
    recalcMaps()
  })
  observeEvent(input$rstr_focal, {
    recalcMaps()
  })
  
  #CB: load Wells ####
  observeEvent(input$wellsfile1, {
    wls <- loadWells(input$wellsfile1)
    if (is.null(wls)) 
      showNotification(ui = "Ошибка при загрузке скважин.
Предполагается следующий формат:
WELL  X_LOCATION  Y_LOCATION",
                       type = "error")
    else {
      showNotification(ui = paste(length(wls[,1])," скважин загружено"),
                       type = "default")
      for( i in 1:length(myReactives$maps))  {
        wls <- extractMap2Well(wls,myReactives$maps[[i]]$rstr, paste0("Map",i))
      }
      myReactives$wells <- wls
      myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
    }
  })
  
  #CB: load CP ####
  observeEvent(input$cpfile1, {
    wls <- loadCP(myReactives$wells,input$cpfile1,cpname = "Values")
    if (is.null(wls)) 
      showNotification(ui = "Ошибка загрузки контрольных точек. Возможно не загружены скважины.
Предполагается следующий формат:
X_LOCATION  Y_LOCATION  VALUE",
                       type = "error")
    else {
      showNotification(ui = paste(length(wls@data$Values[!is.na(wls@data$Values)])," из ",length(wls[,1])," скважин обновлено"),
                       type = "default")
      myReactives$wells <- wls
      myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
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
        map_idx1 = selectMap(maps = myReactives$maps, idx = input$selectMap1)
        map_idx2 = selectMap(maps = myReactives$maps, idx = input$selectMap2)        
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
    #showModal(modalDialog( "Обработка карт...",title = "Ожидайте...", footer = modalButton("Закрыть")))
    mapsSelection =  input$table_maps_rows_selected 
    
    showModDial("Обработка карт...")
    withProgress(message = "обработка карт...", detail = "Ожидайте......", 
                 value =0, {
                selIdx =1
                   
                for(i in 1:length(input$Mapsfile[,1])) {
                  map_obj = loadMapFile(input$Mapsfile[i,], loadFunc = input$mapFormatSel)
                  map_obj = upscaleMap(map_obj,input$rstr_fact,func = flist[[input$focalMethod]],focalSize = input$rstr_focal)
                  
                  if (is.null(map_obj)) 
                     showNotification(ui = "Ошибка при загрузке. Ожидаемый формат - ESRI ASCIIgrid.",
                                        type = "error")
                  else {
                    incProgress(detail = paste0('"',map_obj$fn,'"'), amount = 1./length(input$Mapsfile[,1]))
                    if (length(input$table_maps_rows_selected)>0 && length(myReactives$maps)>1) {
                      map_idx = selectMap(maps = myReactives$maps,sr = input$table_maps_rows_selected, idx = selIdx)
                      
                      if(selIdx<=length(input$table_maps_rows_selected))
                      {
                      myReactives$maps[[map_idx]] <- map_obj
                      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",map_idx))
                      selIdx = selIdx + 1
                      } else {
                        myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",length(myReactives$maps)+1))
                        myReactives$maps <- append(myReactives$maps,list(map_obj))
                      }
                      
                    } else {
                      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",length(myReactives$maps)+1))
                      myReactives$maps <- append(myReactives$maps,list(map_obj))
                    }
                  }
                }
              })
    myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
    myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
    selectRows(dtMapsProxy,as.numeric(mapsSelection))
    updateMapLists(myReactives$maps,input$table_maps_rows_selected)
    removeModal()
  })
  
  #CB: delete maps ####
  observeEvent(input$delmap , {
    #myReactives$maps[[input$table_maps_rows_selected]] = NULL
    #browser()
    
    res = deleteMaps(myReactives$maps,myReactives$wells,sr = input$table_maps_rows_selected)
    #browser()
    myReactives$wells <- res$wells
    myReactives$maps <- res$maps
    if(!is.null(input$table_maps_rows_selected)) {
      #dbgmes("sel0=",input$table_maps_rows_selected)
      #dbgmes("nmap=",length(myReactives$maps))
      sel = input$table_maps_rows_selected
      sel = sel [! sel %in% length(myReactives$maps)]
      if(length(sel<1)) sel = NULL
      # for(sr in 1:length(sel)) {
      #   if(sel[sr]>length(myReactives$maps)) 
      #     sel[sr] = NULL
      # }
      #dbgmes("sel=",sel)
      selectRows(proxy = dtMapsProxy,selected = sel)
      #dbgmes("sel1=",input$table_maps_rows_selected)
    }
    myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps)
    myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
    updateMapLists(myReactives$maps)
  })
  
  #CB: select maps ####
  updateMapLists <- function (maps = NULL, sr = NULL) {
    #browser()
    if(is.null(maps)) return(NULL)
    
    sr = getLiveMapsIds(maps, sr)
    nsr=length(sr)
    for(im in 1:nsr)  
      names(sr)[im]=maps[[sr[im]]]$fn
    
    updateSelectInput(session,"selectMap1",choices = c(sr))#,selected = sel1)
    updateSelectInput(session,"selectMap2",choices = c(sr))#,selected = sel2)
    #dbgmes(message = "choices = ",sr)
    #dbgmes(message = "selected_out = ",c(sel1,sel2))
  }
  
  #CB: MapTable select ####
  #observe({
  #   dbgmes(message = "MapTable sel = ",input$table_maps_rows_selected)
  #   myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps ,sr = input$table_maps_rows_selected)
  #   updateMapLists(maps = myReactives$maps,sr = input$table_maps_rows_selected)
  #   selectRows(proxy = dtMapsProxy,selected = input$table_maps_rows_selected)
  # }, label = "MapTable_select")
  observeEvent(eventExpr = input$table_maps_rows_selected, handlerExpr = {
  #   #browser()
    #dbgmes(message = "selected = ",input$table_maps_rows_selected)
     myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
     myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
   #selectRows(proxy = dtMapsProxy,selected = input$table_maps_rows_selected)
     updateMapLists(maps = myReactives$maps,sr = input$table_maps_rows_selected)

  })
  
  #CB: WellTable select ####
  observeEvent(eventExpr = input$table_wells_rows_selected, handlerExpr = {
    #   #browser()
    #dbgmes(message = "selected = ",input$table_maps_rows_selected)
    #myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
    myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
    #selectRows(proxy = dtMapsProxy,selected = input$table_maps_rows_selected)
    #updateMapLists(maps = myReactives$maps,sr = input$table_maps_rows_selected)
    
  })
  
  #CB: Batch ####
  
  getCCmodel <- reactive({
    showModDial(message = paste0("Оценка эффективности модели ",input$batModel,"..."))
    cc = getModelCCmatrix_par(wells = myReactives$liveWells, minWells = 3,
                                 test_ratio = input$test_ratio,
                                 nnet_complex = input$nnet_complex,
                                 max_iter = input$max_iter,
                                 svmType = input$svmModelType,
                                 NNactFunc = mlpActFuns[[input$nnetActFunc]],
                                 modType = input$batModel
    )
    if(input$batModel=="NNET") {
      myReactives$CCmap_NNET = cc$ccMatrix
      myReactives$CCdset_NNET = cc$ccDset
    }
    else if(input$batModel=="GLM") {
      myReactives$CCmap_GLM = cc$ccMatrix
      myReactives$CCdset_GLM = cc$ccDset
    }
    else if(input$batModel=="SVM") {
      myReactives$CCmap_SVM = cc$ccMatrix
      myReactives$CCdset_SVM = cc$ccDset
    }
    # minCC=round(min(cc$ccMatrix,na.rm = T),digits = 2)-0.1
    # CClimit = input$CClimit
    # updateSliderInput(session,"CClimit",
    #                   min = minCC,
    #                   value = max(c(minCC,CClimit))
    #                   )
    #dbgmes("cc_mat=",cc_matrix)
  })
  showCCmodel <- function (name, data,cc) {
    modalDialog( name,size = "l",
                 tagList(paste0(capture.output(data))
                 )
                 #footer = tagList(
                #   modalButton("Закрыть")
                 #)
    )
  }
  
  observeEvent(input$CCplot_get_model, {
    rc=round(c(input$CCplot_get_model$y,input$CCplot_get_model$x))
    #dbgmes("cc_click=",rc)
    mod = getCurrentModel()
    #browser()
    myReactives$CCtable
    if(all(dim(mod$ccMatrix)>=rc))
    {
      cc = mod$ccMatrix[[rc[1],rc[2]]]
      data = mod$ccDset[[rc[1],rc[2]]][[1]]
      title = c(paste0("Выбранная модель ",input$batModel," (CC=",prettyNum(cc),")"))
      myReactives$CCtable = data
      dbgmes("cc_click=",data)
      #message = paste0(capture.output(as.data.frame(data)),sep = '\n')
      #browser()
      #showModDial(message = message)
      showModal(modalDialog( title = title,size = "m",
                   tagList(fluidRow(tableOutput('CCtable'))),
                   footer = tagList(modalButton("Закрыть")))
      )
    }
  })
  
  output$CCtable <-renderTable(myReactives$CCtable)
  
  output$batPlot <- renderPlot({
    getCCmodel()
    mod = getCurrentModel()
    drawModelCCplot(CCmod = mod,wells = myReactives$liveWells,CClimit = input$CClimit)
    removeModal()
  })
    
  #CB: transposing ####
  observeEvent(input$transpose1 , {
    if(length(myReactives$maps)>1) {
      mapsSelection=input$table_maps_rows_selected
      map_idx = selectMap(maps = myReactives$maps, idx = input$selectMap1)
      map_obj = transposeMap(myReactives$maps[[map_idx]])
      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",map_idx))
      myReactives$maps[[map_idx]] <- map_obj
      myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
      myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
      selectRows(dtMapsProxy,mapsSelection)
      #selectRows(dtMapsProxy,as.numeric(getLiveMapsIds(maps = myReactives$maps, sr = input$table_maps_rows_selected)))
    }
    
  })
  observeEvent(input$transpose2 , {
    if(length(myReactives$maps)>1) {
      mapsSelection=input$table_maps_rows_selected
      map_idx = selectMap(maps = myReactives$maps, idx = input$selectMap2)
      map_obj = transposeMap(myReactives$maps[[map_idx]])
      myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",map_idx))
      myReactives$maps[[map_idx]] <- map_obj
      myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
      myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
      selectRows(dtMapsProxy,as.numeric(mapsSelection))
    }
  })
  
  #CB: upscaling  ####
  observeEvent(input$rstr_fact , {
    recalcMaps()
    return()
    #showModal(modalDialog( "Обработка карт...",title = "Ожидайте...", footer = modalButton("Закрыть")))
    showModDial("Обработка карт...")
    mapsSelection=input$table_maps_rows_selected
    
    withProgress(message = "Обработка карт...", detail = "Ожидайте...",  value =0, {
      recalcMaps()
    #   for (i in 1:length(myReactives$maps)) {
    #   map_obj = upscaleMap(myReactives$maps[[i]],input$rstr_fact,func = flist[[input$focalMethod]],focalSize = input$rstr_focal)
    #   incProgress(detail = paste0('"',map_obj$fn,'"'), amount = 1./length(myReactives$maps))
    #   myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",i))
    #   myReactives$maps[[i]] <- map_obj
    # }
      })
    #myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = input$table_maps_rows_selected)
    selectRows(dtMapsProxy,as.numeric(mapsSelection))
    #dbgmes(message = "upscaling=",c(length(myReactives$liveMaps),length(myReactives$maps)))
    removeModal()
  })
  
  #CB: plot maps ####
  output$mapPlot1 <- renderPlot({
    if(is.null(myReactives$wells)) {
      myReactives$wells <- wells0
      for (i in 1:length(myReactives$maps)) 
        myReactives$wells <- extractMap2Well(myReactives$wells,myReactives$maps[[i]]$rstr, paste0 ("Map",i))
    }
    map_idx = selectMap(maps = myReactives$maps, idx = input$selectMap1)
    #dbgmes(message = "map_idx=",map_idx)
    if(is.na(map_idx)) return()
    #browser()
    drawRstr(map = myReactives$maps[[map_idx]]$rstr,zoom = myReactives$zoom, 
             pal = mapPalList[[input$mapPalSelect1]],
             alpha = input$mapPalAlpha1,
             contours = input$showContours1,
             interpolate = input$interpMap1)
    drawWells(myReactives$wells, myReactives$maps[[map_idx]]$rstr, sr = input$table_wells_rows_selected,srmap = input$table_maps_rows_selected)
  })
  
  output$mapPlot2 <- renderPlot({
    if(length(myReactives$maps)>1) {
      map_idx = selectMap(maps = myReactives$maps, idx = input$selectMap2)
      drawRstr(myReactives$maps[[map_idx]]$rstr,myReactives$zoom, 
               pal = mapPalList[[input$mapPalSelect2]],
               alpha = input$mapPalAlpha2,
               contours = input$showContours2,
               interpolate = input$interpMap2)
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
    map_idx = selectMap(maps = myReactives$maps, idx = input$selectMap1)
    drawHist(myReactives$maps[[map_idx]], input$bins)
  })
  output$histPlot2 <- renderPlot({
    map_idx = selectMap(maps = myReactives$maps, idx = input$selectMap2)
    drawHist(myReactives$maps[[map_idx]], input$bins)
  })
  
  #CB: hexbin plot xplot ####
  output$xPlot_hex <- renderPlot({
    map_idx1 = selectMap(maps = myReactives$maps, idx = input$selectMap1)
    map_idx2 = selectMap(maps = myReactives$maps, idx = input$selectMap2)
    drawHex(getXYvectors(myReactives$maps[[map_idx1]], myReactives$maps[[map_idx2]]), input$cells)
  })
  
  #CB: NNET calc model ####
  recalcNNET <- reactive ({
    showModDial("Обучение нейронной сети MLP...")
    actFuncType <- mlpActFuns[[input$nnetActFunc]]
    #     myReactives$liveWells <- prepDataSet(wells = myReactives$wells, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
    dbgmes("liveWells=",myReactives$liveWells)
    myReactives$nnet <- buildNNET(myReactives$liveWells,#myReactives$wells,
                                  #input$table_wells_rows_selected, input$table_maps_rows_selected,
                                  test_ratio = input$test_ratio/100.,
                                  max_iter = input$max_iter,
                                  nnet_complex = input$nnet_complex/100.,
                                  actFunc = actFuncType)
  })
  remapNNET <- reactive ({
      #dbgmes("sel=",input$nnetActFunc)
    #dbgmes("res=",mlpActFuns[[input$nnetActFunc]])
    myReactives$nnet_map = drawNNETmap(data = myReactives$liveMaps,
                                        sr = input$table_maps_rows_selected,
                                        nnet = myReactives$nnet)
    
  })
  #CB: NNET plot model ####
  output$nnetPlot <- renderPlot({
    recalcNNET()
    main = input$main
    mod = getCurrentModel()
    model=mod$model
    mode = mod$mode
    mode_aux = mod$mode_aux
    resmap = mod$map
    title = mod$title
    
    if(is.null(model) && is.null(resmap))
    {
      removeModal()
      plotError("Пустая выборка.\n Проверьте согласованность координат скважин и карт")
      return(NULL)
    }
    
    #browser()
    
    if(mode == "res"){
      remapNNET()
      mapPar=getMapPar()
      drawModMap(datplot = resmap,title = unlist(title),
                 colors = mapPar$col,
                 zoom = myReactives$zoom,interpolate = mapPar$interp)
      drawWells(wells = myReactives$wells, 
                sr = input$table_wells_rows_selected,
                srmap = input$table_maps_rows_selected)
    } else {
      drawNNETmodel(nnet = myReactives$nnet, mode = input$nnetAuxMode)
    }
    removeModal()
  })
  output$nnetText <- renderText({ #renderPrint
    #frm = getModelText(myReactives$nnet)
    if(is.null(myReactives$nnet)) return(NULL)
    measured = myReactives$nnet$inp
    predicted = myReactives$nnet$out
    lmr = lm(formula = predicted~measured)
    frm = getModelXplotText(lmfit = lmr)
    paste(frm)
  })

  putMap2inp <- function() {
    mod = getCurrentModel()
    #browser()
    map_obj = loadMapFile(spgid = rstr2spgrid(mod$map))
    #isolate({
    map_obj$fn = mod$title
    if(is.null(map_obj)) {
      showNotification(ui = paste(map_obj$fn," не удалось добавить карту во входные."),
                       type = "error")
    } else {
      mapsSelection =  input$table_maps_rows_selected 
      if(is.null(mapsSelection))
        mapsSelection = getLiveMapsIds(maps = myReactives$maps, sr = input$table_maps_rows_selected)
      #dbgmes("sel=",mapsSelection)
      myReactives$maps <- append(myReactives$maps,list(map_obj))
      isolate({myReactives$wells <- extractMap2Well(myReactives$wells,map_obj$rstr, paste0 ("Map",length(myReactives$maps)))})
      selectRows(dtMapsProxy,as.numeric(mapsSelection))
      updateMapLists(myReactives$maps,mapsSelection)#input$table_maps_rows_selected)
      myReactives$liveMaps <- getLiveMapsData(maps = myReactives$maps, sr = mapsSelection)#input$table_maps_rows_selected)
      myReactives$liveWells <- prepDataSet(wells = myReactives$wells@data, rows =input$table_wells_rows_selected  ,sel_maps =  input$table_maps_rows_selected)
      showNotification(ui = paste("Карта",map_obj$fn," добавлена во входные."),
                       type = "default")
    }
    #})
  }
  
  observeEvent(input$addModMap2inp , {
    putMap2inp()
  })
  
  #CB: SVM calc model ####
  recalcSVM <- reactive ({
    showModDial("Построение модели SVM...")
    svmType <- input$svmModelType
    myReactives$svm <- buildSVM(myReactives$liveWells,
                                  test_ratio = input$test_ratio/100.,
                                  type = svmType)
    #dbgmes("sel=",input$nnetActFunc)
    #dbgmes("res=",mlpActFuns[[input$nnetActFunc]])
    myReactives$svm_map = drawSVMmap(data = myReactives$liveMaps,
                                       sr = input$table_maps_rows_selected,
                                       svm = myReactives$svm)
    
  })
  #CB: SVM plot model ####
  output$svmPlot <- renderPlot({
    recalcSVM()
    main = input$main
    mod = getCurrentModel()
    model=mod$model
    mode = mod$mode
    mode_aux = mod$mode_aux
    resmap = mod$map
    title = mod$title
    
    if(is.null(model))
    {
      removeModal()
      plotError("Пустая выборка.\n Проверьте согласованность координат скважин и карт")
      return(NULL)
    }
    
    #browser()
    
    if(mode == "res"){
      mapPar=getMapPar()
      drawModMap(datplot = resmap,title = unlist(title),
                 colors = mapPar$col,
                 zoom = myReactives$zoom,interpolate = mapPar$interp)
      drawWells(wells = myReactives$wells, 
                sr = input$table_wells_rows_selected,
                srmap = input$table_maps_rows_selected)
    } else if(mode == "xplot") {
      drawModelXplot (myReactives$liveWells, model, input$table_wells_rows_selected)
    } else {
      #drawSVMmodel(model = myReactives$svm, mode = input$nnetAuxMode)
      #browser()
      #par(mfrow=c(1,1))
      #par(new = TRUE)
      if(!is.null(myReactives$svm$tune))
      plot(myReactives$svm$tune,
           main = c("Лучшая модель\n",
                        paste(names(myReactives$svm$tune$best.parameters),
                              myReactives$svm$tune$best.parameters))
           )
      else 
        plotError("asd")
      #points(myReactives$svm$tune$best.parameters,pch = 16)
      #text(myReactives$svm$tune$best.parameters,"Best", pos=1)
      
      #if(myReactives$svm$class) plot(myReactives$svm$mod,data = model$SV, formula = Values~.)
      #else plotError(message = "Для SVM регресии нет просмотра модели")
    }
    removeModal()
  })
  output$svmText <- renderText({ #renderPrint
    #frm = getModelText(myReactives$nnet)
    if(is.null(myReactives$nnet)) return(NULL)
    measured = myReactives$nnet$inp
    predicted = myReactives$nnet$out
    lmr = lm(formula = predicted~measured)
    frm = getModelXplotText(lmfit = lmr)
    paste(frm)
  })
  #CB: GLM calc model ####
  recalcGLM <- reactive({
    showModDial("Создание модели многомерной линейной регрессии...")
    myReactives$glm <- buildGLM(myReactives$liveWells 
                                #input$table_wells_rows_selected, input$table_maps_rows_selected
                                )
    mapPar = getMapPar()

    myReactives$glm_map= drawGLMmap(data = myReactives$liveMaps,
                                    sr = input$table_maps_rows_selected,
                                    glm = myReactives$glm,
                                    colors = mapPar$col)      
  })
  #CB: GLM plot model ####
  output$glmPlot <- renderPlot({
    recalcGLM()
    main = input$main
    mod = getCurrentModel()
    model=mod$model
    mode = mod$mode
    mode_aux = mod$mode_aux
    resmap = mod$map
    title = mod$title
    
    #browser()
    if(is.null(model)) {
      removeModal()
      plotError("Пустая выборка.\n Проверьте согласованность координат скважин и карт")
      return(NULL)
    }
    
    if(mode == "mod") {    
      par(mfrow = c(2,2))
      plot(model)
    } else if(mode =="xplot") {
      dbgmes("class=",class(model))
      drawModelXplot (myReactives$liveWells, myReactives$glm, input$table_wells_rows_selected)
    } else if(mode =="res") {
      #colors = mapPalList[["Радуга"]](128)
      mapPar = getMapPar()

      drawModMap(datplot = resmap,title = unlist(title),
                 colors = mapPar$col,
                 zoom = myReactives$zoom,interpolate = mapPar$interp)
      drawWells(wells = myReactives$wells, 
                sr = input$table_wells_rows_selected,
                srmap = input$table_maps_rows_selected)
    }
#    observe(myReactives$glm <- fit)
    removeModal()
  })
  output$glmText <- renderText({ #renderPrint
    mode = mode = input$glmAuxMode
    if(mode == "mod")
      txt = getModelText(myReactives$glm)
    else if( mode == "xplot")
      txt = getModelXplotText (myReactives$wells@data, myReactives$glm, input$table_wells_rows_selected)    
    else txt = "todo"
    paste(txt)
  })
  
  output$modelsXText <- renderText({
    paste0(capture.output(summary(getCurrentModel()$model)),"\n")
  })
  
  recalcKMeans <- reactive({
    #dbgmes(message = "maps=",myReactives$liveMaps)
    showModDial("Кластеризация K-means...")
    myReactives$km <- calcKMeans(myReactives$liveMaps,input$numClasses)#,sr = input$table_maps_rows_selected)
  })
  
  #CB: KM plot model ####
  output$kmPlot <- renderPlot({
    recalcKMeans()
    drawModel(myReactives$liveMaps,myReactives$km)
    removeModal()
  })

  output$kmText <- renderText({ #renderPrint renderText
    paste0(capture.output(myReactives$km),"\n")
  })

  output$kmXPlot <- renderPlot({
    recalcKMeans()
    myReactives$km_map = drawModMapPlot(myReactives$liveMaps,sr = input$table_maps_rows_selected,myReactives$km)
    par(new = TRUE)
    drawWells(wells = myReactives$wells, 
              sr = input$table_wells_rows_selected,
              srmap = input$table_maps_rows_selected)
    removeModal()
  })

  output$kmXText <- renderText({ #renderPrint renderText
    getModelMapText(myReactives$km_map)
  })
  
  #CB: KM download  ####
  output$downloadKMMap <- downloadHandler(
    filename = getMapSaveFilename,
    contentType = '.asc',
    content = getMapSaveContent
  )
  observeEvent(input$addKMMap2inp , {
    putMap2inp()
  })

  #CB: GMM model ####    
  recalcGMM <- reactive ({
    if(input$numClassUD) nClasses = 0
    else nClasses = input$numClasses
    showModDial("Кластеризация Gaussian Mixture - EM...")
    myReactives$gmm <- calcGMM(myReactives$liveMaps,nClasses)
    #myReactives$gmm <- calcGMM(myReactives$liveMaps,nClasses)
    if(input$numClassUD)
      updateSliderInput(session,"numClasses",value = myReactives$gmm$G)
    return(myReactives$gmm)
  })
  
  #CB: GMM plot model ####
  output$gmmPlot <- renderPlot({
    recalcGMM()
    drawModel(myReactives$liveMaps,myReactives$gmm)
    removeModal() 
  })

  output$gmmText <- renderText({ #renderPrint renderText
    paste0(capture.output(summary(myReactives$gmm)),"\n")
  })

  output$gmmXText <- renderText({ #renderPrint renderText
    getModelMapText(myReactives$gmm_map)
  })
  
  output$gmmXPlot <- renderPlot({
    recalcGMM()
    myReactives$gmm_map = drawModMapPlot(myReactives$liveMaps,sr = input$table_maps_rows_selected,
                                         myReactives$gmm)
    par(new = TRUE)
    drawWells(wells = myReactives$wells, 
              sr = input$table_wells_rows_selected,
              srmap = input$table_maps_rows_selected)
    removeModal() 
  })
  
  output$gmAuxPlot <- renderPlot({
    recalcGMM()
    drawModBIC(model = myReactives$gmm,mode = input$gmmAuxMode)
    removeModal() 
  })
  
  recalcSVMC <- reactive ({
    showModDial("Кластеризация SVM one-class...")
    myReactives$svcm = calcSVMC(myReactives$liveMaps,type = NULL)
  })
  
  redrawSVMC <- reactive ({
    #showModDial("Кластеризация SVM one-class...")
    #browser()
    #data = myReactives$liveMaps[-1]
    myReactives$svcm$predicted = as.integer(predict(myReactives$svcm))+1
    drawModel(data = myReactives$liveMaps, model = myReactives$svcm)
    #cls = as.integer(predict(myReactives$svcm))+1
    #colors = setPaletteTransp(classPalette,getDataTransp(data))
    #plot(data,col = colors[cls],pch = 16, main = "SVM one-class clustering")
  })
  #CB: SVMC model ####    
  output$svPlot <- renderPlot({
    recalcSVMC()
    redrawSVMC()
    removeModal() 
  })
  
  output$svXPlot <- renderPlot({
    #data = myReactives$liveMaps[-1]
    #dbgmes("data = ",data)
    #myReactives$svcm = svm(data)
    #cls = as.integer(predict(myReactives$svcm$mod))+1
    myReactives$svcm_map = drawModMapPlot(myReactives$liveMaps,sr = input$table_maps_rows_selected,
                                          myReactives$svcm)
    drawWells(wells = myReactives$wells, 
              sr = input$table_wells_rows_selected,
              srmap = input$table_maps_rows_selected)
    removeModal() 
  })
  
  
  getMapPar <- function()
  {
    if(input$mapsTabs == "mapTab1") 
    {
      interp = input$interpMap1
      colors = mapPalList[[input$mapPalSelect1]](128)
      alpha = input$mapPalAlpha1
    }
    else if (input$mapsTabs == "mapTab2") 
    {
      interp = input$interpMap2
      colors = mapPalList[[input$mapPalSelect2]](128)
      alpha = input$mapPalAlpha2
    }
    else {
      colors = rainbow(128)
      interp = T
      alpha = 0.5
    }
    colors = setPaletteTransp(colors,1-alpha)
    return(list(col = colors,interp = interp))
  }
  #CB: Map download ####
  getCurrentModel <- function () {
    main = input$main
    model = NULL
    mode = NULL
    mode_aux = NULL
    resmap = NULL
    prediction = NULL
    cc = NULL
    dset = NULL
    title = ""
    if(main == 'maps') {
      if(input$maps == 'modelKM') {
        #recalcGMM()
        model = myReactives$km
        mode = input$modelKM
        resmap = myReactives$km_map
        title = (capture.output(model))[1]
        prediction = model$cluster
      } else if (input$maps == 'modelGM') {
        model = myReactives$gmm
        mode = input$modelGM
        mode_aux = input$gmmAuxMode
        resmap = myReactives$gmm_map
        title = (capture.output(summary(model)))[2]
        prediction = model$classification
      } else if (input$maps == 'modelHC') {
        model = myReactives$hcm
        mode = input$modelHC
        resmap = myReactives$hcm_map
        prediction = model$predicted
        title = paste("Hierarhical Clusteing",(capture.output(model))[5:6])
      } else if (input$maps == 'modelSV') {
        model = myReactives$svmc
        mode = input$modelSV
        resmap = myReactives$svmc_map
        prediction = model$predicted
        title = paste("One-class clustering (ouliers)",(capture.output(model))[1])
      }
    } else if (main == 'models') {
      if(input$models == 'nnet') {
        model = myReactives$nnet
        mode = mode_aux = input$nnetAuxMode
        resmap = myReactives$nnet_map
        title = capture.output(summary(model$net))[1]
      } else if(input$models == 'glm') {
        model = myReactives$glm
        mode = mode_aux = input$glmAuxMode
        resmap = myReactives$glm_map
        title = "Generalized multiple linear regression"
      } else if(input$models == 'svm') {
        model = myReactives$svm$mod
        mode = mode_aux = input$svmAuxMode
        resmap = myReactives$svm_map
        title = "Support Vector Machine model"
      }
    } else if (main == 'batch') {
      if(input$batModel=="NNET") {
        cc = myReactives$CCmap_NNET
        dset = myReactives$CCdset_NNET
      }
      else if(input$batModel=="GLM") {
        cc = myReactives$CCmap_GLM
        dset = myReactives$CCdset_GLM
      }
      else if(input$batModel=="SVM") {
        cc = myReactives$CCmap_SVM
        dset = myReactives$CCdset_SVM
      }
    }
    #browser()
    return(list(map = resmap,model = model, title = title,
                mode = mode,mode_aux = mode_aux,ccMatrix = cc, ccDset = dset))
  }
  #CB: Map download ####
  getMapSaveFilename <- function() {
    model = getCurrentModel()$model
    paste0(class(model),'_',Sys.Date(), '.asc')
  }
  getMapSaveContent <- function(fname) {
    map = getCurrentModel()$map
    outgrid = saveModMap(map,
                         fname = fname,
                         format = input$mapFormatSel)
  }
  
  output$downloadModMap <- downloadHandler(
    filename = getMapSaveFilename,
    contentType = '.asc',
    content = getMapSaveContent
  )
  #CB: GMM download  ####
  output$downloadGMMap <- downloadHandler(
    filename = getMapSaveFilename,
    contentType = '.asc',
    content = getMapSaveContent
  )
  observeEvent(input$addGMMap2inp , {
    putMap2inp()
  })
  
  #CB: HC model ####    
  recalcHCM <- reactive ({
    showModDial("Иерархическая кластеризация...")
    hcm <- calcHC(myReactives$liveMaps[-1],
                              mode = hcmModes_hcluster[[input$hcMode]],
                              distMode = hcmDistModes[[input$hcDistMode]])
    #cutree(model$mod,min(nClass,length(myReactives$hcm$height)))
    #browser()
    # resmap = drawModMapPlot(myReactives$liveMaps,sr = input$table_maps_rows_selected,
    #                         hcm,
    #                         nClass = input$numClasses,
    #                         doPlot = FALSE)
    # hcm$predicted = resmap@data@values
    myReactives$hcm = hcm
    #myReactives$hcm$predicted = resmap@data@values
    removeModal() 
    #return(myReactives$hcm)
   # if(input$numClassUD)
    #  updateSliderInput(session,"numClasses",value = myReactives$gmm$G)
  })
  
  #CB: HC plot model ####
  output$hcPlot <- renderPlot({
    recalcHCM()
    drawModel(myReactives$liveMaps,myReactives$hcm, nclass = input$numClasses)
  })
  
  output$hcText <- renderText({ #renderPrint renderText
    paste0(capture.output(summary(myReactives$hcm_map)),"\n")
  })
  
  output$hcXPlot <- renderPlot({
    recalcHCM()
    myReactives$hcm_map = drawModMapPlot(myReactives$liveMaps,sr = input$table_maps_rows_selected,
                                         myReactives$hcm,
                                         nClass = input$numClasses)
    par(new = TRUE)
    drawWells(wells = myReactives$wells, 
              sr = input$table_wells_rows_selected,
              srmap = input$table_maps_rows_selected)
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
                           sr = input$table_maps_rows_selected,
                           fname = fname,
                           format = input$mapFormatSel)
      #save = try( expr = write.asciigrid(outgrid,fname) , TRUE)
      #if(class(save)=="try-error")
      #  showNotification(ui = "Ошибка при сохранении файла",
      #                   type = "error")      
    }
  )
  observeEvent(input$addHCMap2inp , {
    putMap2inp()
  })
  
  # CB: Zoom maps on Click ####
  zoomPlotCall <- function (name, data) {
    modalDialog( name,size = "l",
                 tagList(plotOutput("zoomPlot", 
                                    dblclick = "plot_zoom_close",
                                    brush = "plot_zoom_brush",
                                    hover = hoverOpts( 
                                      id ="plot_zoom_hover",
                                      delay =100),
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
    #dbgmes(message = "xy=",xy)
    labs = c(" X:","Y:","Z:")
    if(length(xy) == 0) {
      lab=paste0(labs,xy)
      updateActionButton(session,"plot_zoom_xyz",label = lab)
    }
    else 
    {
      coordinates(xy) = ~x+y
      map = getCurrentModel()$map
      if(!is.null(map))  xy$z = extract(map,xy)
      else xy$z = 0
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
  
  # CB: Zoom Plot draw ####
  output$zoomPlot <- renderPlot({
    main = input$main
    mod = getCurrentModel()
    model=mod$model
    mode = mod$mode
    mode_aux = mod$mode_aux
    resmap = mod$map
    title = mod$title
    
    #browser()
    
    if(main == 'maps') {
    if (!is.null(model)) {
      if(mode == 'res'){
        colors = classPalette[1:resmap@data@max]
        colors = setPaletteTransp(colors,0.5)
        drawModMap(datplot = resmap,title = title,
                   colors = colors,
                   zoom = myReactives$plot_zoom,interpolate = F)
        par(new = TRUE)
        drawWells(wells = myReactives$wells, 
                  sr = input$table_wells_rows_selected,
                  srmap = input$table_maps_rows_selected)
      } else if(mode == 'mod') {
        drawModel(myReactives$liveMaps,model, nclass = input$numClasses)
      } else if(mode == 'aux') {
        drawModBIC(model,mode = mode_aux)
      }
    }
    } else if (main == 'models') {
      if(input$models == 'nnet') {
        if( mode_aux != 'res')
          drawNNETmodel(nnet = model , mode = mode)
      } else if(input$models == 'glm') {
        if(mode == 'mod'){
          par(mfrow = c(2,2))
          plot(model)
        } else if (mode == 'xplot') {
          drawModelXplot (myReactives$wells@data, model, input$table_wells_rows_selected)
        }
      }
      if( mode == 'res'){
        mapPar = getMapPar()
        drawModMap(datplot = resmap,title = title,
                    colors = mapPar$col,
                    zoom = myReactives$plot_zoom,interpolate = mapPar$interp)
        drawWells(wells = myReactives$wells, 
                  sr = input$table_wells_rows_selected,
                  srmap = input$table_maps_rows_selected)
      }
    }
  })
  
  #CB: plot simple xplot ####
  output$xPlot_simple <- renderPlot({
    map_idx1 = selectMap(maps = myReactives$maps, idx = input$selectMap1)
    map_idx2 = selectMap(maps = myReactives$maps, idx = input$selectMap2)
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
    map_idx = selectMap(maps = myReactives$maps, idx = input$selectMap1)
    if(is.null(map_idx) || is.null(myReactives$maps[[map_idx]])) paste('Проверьте выбор карт')
    else basename(myReactives$maps[[map_idx]]$fn)
  }))
  output$tab2 = renderText(({
    map_idx = selectMap(maps = myReactives$maps, idx = input$selectMap2)
    if(is.null(map_idx) || is.null(myReactives$maps[[map_idx]])) paste('Проверьте выбор карт')
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
