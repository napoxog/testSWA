#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#default data definition
map0 <- sp::read.asciigrid("d:/soft.old/R/Data/Maps/default.asc")
fn0 <- "default.asc"
names(map0@data) <- fn0
def_map <- list("fn" = fn0,
                "map" = map0,
                "mat" = as.matrix(map0))
wells0 = NULL

map_hei = 600
map_wid = 800
hist_hei = 200
hist_wid = 300

myReactives <- reactiveValues(map1 = def_map, map2 = def_map,wells = wells0)

library(shiny)
library(readr)

library(spatial)
library(sp)
library(hexbin)
library(markdown)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Кроссплоты карт"),
  # test = enc2utf8(c("привет","пока"))
  
  
  #UI: Sidebar with a slider input for number of bins ####
  sidebarLayout(
    sidebarPanel(
      plotOutput(
        "xPlot_hex",
        click = "plot_click",
        dblclick = "plot_dblclick",
        hover = "plot_hover",
        brush = "plot_brush"
      ),
      sliderInput(
        "cells",
        "размер бина кроссплота:",
        min = 1,
        max = 50,
        value = 30
      ),
      plotOutput(
        "xPlot_simple",
        click = "plot_click",
        dblclick = "plot_dblclick",
        hover = "plot_hover",
        brush = "plot_brush"
      )
      ,width = 4, fluid = FALSE),
    #), width = 3 ), #
    mainPanel(
      splitLayout(
        #UI: Wells inputs and sliders ####
        fileInput(
          "wellsfile1",
          "Открыть скважины:",
          accept = c("text/asc",
                     "text/esri-asciigrid,text/plain",
                     "*.asc",
                     buttonLabel = "Открыть...",width = '100%')
        ),
        #UI: Status panel ####
        verbatimTextOutput("info"),
        sliderInput(
          "bins",
          "Количество бинов:",
          min = 1,
          max = 100,
          value = 30
        )
        ,width = 8, fluid = FALSE),
      # UI: Tabs with Maps and related ####
      tabsetPanel(
        tabPanel(
          #UI: Tab1 ####
          uiOutput("tab1"),
          splitLayout(
            verticalLayout(
              fileInput(
                "file1",
                "Открыть сетку 1:",
                accept = c("text/asc",
                           "text/esri-asciigrid,text/plain",
                           ".asc",
                           buttonLabel = "Открыть...")
              ),
              actionButton("trans1", "Транспонировать")
            ),
            plotOutput("histPlot1",  
                       height = hist_hei,
                       width = hist_wid
            )
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
        ),
        tabPanel(
          #UI: Tab2 ####
          uiOutput("tab2"),
          splitLayout(
            verticalLayout(
              fileInput(
                "file2",
                "Открыть сетку 2:",
                accept = c("text/asc",
                           "text/esri-asciigrid,text/plain",
                           ".asc",
                           buttonLabel = "Открыть...",width = "10%")
              ),
              actionButton("trans2", "Транспонировать")
            ),
            plotOutput("histPlot2", 
                       height = hist_hei,
                       width = hist_wid
            )
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
    )
    
  )
)

## transpose the maps' matrix 
transposeMap <- function(map_obj) {
  map_m = matrix(map_obj$map@data[[1]], ncol = map_obj$map@grid@cells.dim[1], nrow = map_obj$map@grid@cells.dim[2], byrow = TRUE)
  map_obj$map@data = data.frame(as.vector(map_m))
  map_obj$map@grid@cells.dim = c(map_obj$map@grid@cells.dim[2],map_obj$map@grid@cells.dim[1])
  map_obj$mat = as.matrix(map_obj$map)
  return(map_obj)
}

## load map file in ESRI ascii grid format, with optional transposing of the matrix ##
loadMapFile <- function (file_obj, transpose = FALSE) {
  if (is.null(file_obj)) {
    map = try( expr = sp::read.asciigrid("d:/soft.old/R/Data/Maps/default.asc") , TRUE)
    if(class(map)=="try-error"){
      return(NULL)
    }
    map_m = as.matrix(map)
    fn = "default.asc"
  } else {
    map = try( expr = sp::read.asciigrid(file_obj$datapath), TRUE)
    if(class(map)=="try-error"){
      return(NULL)
    }
    map_m = matrix(map@data[[1]], ncol = map@grid@cells.dim[2], nrow = map@grid@cells.dim[1])
    fn = file_obj$name
  }
  names(map@data) <- fn
  map_obj = list(
    "fn" = fn,
    "map" = map,
    "mat" = map_m
  )
  #transpose the data.frame
  if(transpose) {
    map_obj = transposeMap(map_obj)
  }
  return(map_obj)
}

## load wells coordinates for display and store the CP data ##
loadWells <- function(file_obj) {
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
  #browser()
  if( class(wells)=="try-error" || 
      dim(ers)[1]>0 || 
      !(c("X_LOCATION","Y_LOCATION") %in% names(wells) ) 
      ) 
    return(NULL)
  
  coordinates(wells) = ~X_LOCATION+Y_LOCATION
  return(wells)
}

# load CP data into the current wells coordinates or create wells from coordiantes in the file
loadCP <- function (wells = NULL, cpFile , tolerance = 10) {
  cpdata = read_table2(cpFile,col_names = F)
  ers <- try (expr = problems(wells),TRUE)
  
  coordinates(cpdata) = ~X1+X2
  
  #browser()
  if( class(cpdatacpdata)=="try-error" || 
      dim(ers)[1]>0 || 
      !(c("X_LOCATION","Y_LOCATION") %in% names(wells) ) 
  ) 
    return(NULL)

  return(wells)
}

# Draw single map
drawMap <- function (map,wells) {
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
  #  spplot(
  #    map,
  #    sp.layout = list(arrow, scale, text1, text2, arrow),
  #    scales = list(draw = T),
  #    xlim = bbexpand(bbox(map)[1,], 0.3),
  #    ylim = bbexpand(bbox(map)[2,], 0.1),
  #    colorkey = list(space = "right", height = 0.4)
  #  )
  plot(map)
}

drawWells <- function(wells) {
  if(!is.null(wells)) {
    #par(new = TRUE)
    plot(wells, add = TRUE, pch = 21)
    text(wells, add = TRUE, labels = wells$WELL, cex = .7, pos = 4)
  }
  
}


drawHist <- function (map, nbins) {
  map_m <- as.matrix(map)
  x <- map_m[!is.na(map_m)]
  bins <- seq(min(x), max(x), length.out = nbins + 1)
  # draw the histogram with the specified number of bins
  hist(
    x,
    breaks = bins,
    col = 'darkgray',
    border = 'white',
    main = paste("Гистограмма " , names(map))
  )
}

#match and return vectors for CC and xplot calc
getXYvectors <- function(mat1, mat2) {
  mask <- mat1 + mat2
  mask[!is.na(mask)] <- TRUE
  mask[is.na(mask)] <- FALSE
  
  x1 <- mat1[as.logical(mask)]
  x2 <- mat2[as.logical(mask)]
  #browser()
  return (list("x" = x1, "y" = x2))
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  #increase max file size
  options(shiny.maxRequestSize = 500 * 1024 ^ 2)
  options(shiny.reactlog = TRUE)
  options(shiny.host = "0.0.0.0")
  options(shiny.port = 8080)
  
  #check if files are selected
  observeEvent(input$file1 , {
    observe(myReactives$map1 <- loadMapFile(input$file1))
  })
  
  observeEvent(input$file2, {
    observe(myReactives$map2 <- loadMapFile(input$file2))
  })
  
  observeEvent(input$wellsfile1, {
    observe(myReactives$wells <- loadWells(input$wellsfile1))
  })
  
  observeEvent(input$trans1 , {
    map_obj = transposeMap(myReactives$map1)
    observe(myReactives$map1 <- map_obj)
  })
  
  observeEvent(input$trans2 , {
    map_obj = transposeMap(myReactives$map2)
    observe(myReactives$map2 <- map_obj)
  })
  
  
  #plot maps
  output$mapPlot1 <- renderPlot({
    drawMap(myReactives$map1$map)
    drawWells(myReactives$wells)
  })
  output$mapPlot2 <- renderPlot({
    drawMap(myReactives$map2$map,myReactives$map1$wells)
    drawWells(myReactives$wells)
  })
  
  
  #plot histograms
  output$histPlot1 <- renderPlot({
    drawHist(myReactives$map1$mat, input$bins)
  })
  output$histPlot2 <- renderPlot({
    drawHist(myReactives$map2$mat, input$bins)
  })
  
  # plot hexbin xplot
  output$xPlot_hex <- renderPlot({
    xy <- getXYvectors(myReactives$map1$mat, myReactives$map2$mat)
    xccf <- ccf(xy$x, xy$y, lag.max = 0)
    hbin <- hexbin(xy$x, xy$y, xbins = input$cells + 1)
    plot(
      hbin,
      xlab = basename(myReactives$map1$fn),
      ylab = basename(myReactives$map2$fn),
      main = sprintf("Кроссплот CC=%5.2f %s=%5.2f", xccf$acf, parse(text = 'R^2'), as.numeric(xccf$acf) ^ 2)
    )
  })
  output$xPlot_simple <- renderPlot({
    xy <- getXYvectors(myReactives$map1$mat, myReactives$map2$mat)
    xccf <- ccf(xy$x, xy$y, lag.max = 0)
    
    xmean <- mean(xy$x)
    xstd <- sd(xy$x)
    xdv <- max(xy$x)-min(xy$x)
    
    ymean <- mean(xy$y)
    ystd <- sd(xy$y)
    ydv <- max(xy$y)-min(xy$y)
    
    dms <- sqrt((2*xstd)^2+(2*ystd)^2)
    dmd <- sqrt(xdv^2+ydv^2)
    dm <- abs(as.numeric(xccf$acf))*(dms/dmd)
    #mcol - expected max number of points in a cell
    #mcol <- length(xy$x)^(dm)
    mcol <- ceiling(length(xy$x)^sqrt(dm))*2
    plot(
      xy$x,
      xy$y,
      #xlab = basename(myReactives$map1$fn),
      #ylab = basename(myReactives$map2$fn),
      xlab = mcol,
      ylab = dm,
      main = sprintf("Кроссплот CC=%5.2f %s=%5.2f", xccf$acf, parse(text = 'R^2'), as.numeric(xccf$acf) ^ 2),
      col = rgb(0, 0, mcol, 1, maxColorValue = mcol),
      pch = 16
    )
    
  })
  
  output$tab1 = renderText(({
    basename(myReactives$map1$fn)
  }))
  
  output$tab2 = renderText(({
    basename(myReactives$map2$fn)
  }))
  
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
