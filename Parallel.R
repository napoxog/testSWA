library(shiny)
library(parallel)

#
# reactive variables
# 
rVal <- reactiveValues()
rVal$process <- NULL
rVal$msg <- NULL
rVal$obs <- NULL
counter <- 0
results <- list()
dfEmpty <- data.frame(results = numeric(0))


#
# Long computation
#
analyze <- function() {
  out <- mclapply(1:5, function(x) {
    Sys.sleep(1)
    rnorm(1)
  })
  data.frame(results = unlist(out))
}

#
# Shiny app
#
shinyApp(
  ui = fluidPage(
    column(6,
           wellPanel(
             tags$label("Press start and wait 5 seconds for the process to finish"),
             actionButton("start", "Start", class = "btn-primary"),
             actionButton("stop", "Stop", class = "btn-danger"),
             textOutput('msg'),
             tableOutput('result')
           )
    ),
    column(6,
           wellPanel(
             sliderInput(
               "inputTest",
               "Shiny is responsive during computation",
               min = 10,
               max = 100,
               value = 40
             ),
             plotOutput("testPlot")
           ))),
  server = function(input, output, session)
  {
    #
    # Add something to play with during waiting
    #
    output$testPlot <- renderPlot({
      plot(rnorm(input$inputTest))
    })
    
    #
    # Render messages
    #
    output$msg <- renderText({
      rVal$msg
    })
    
    #
    # Render results
    #
    output$result <- renderTable({
      print(rVal$result)
      rVal$result
    })
    
    #
    # Start the process
    #
    observeEvent(input$start, {
      if (!is.null(rVal$process))
        return()
      rVal$result <- dfEmpty
      rVal$process <-   analyze()
      
      rVal$msg <- sprintf("%1$s started", rVal$process$pid)
      
    })
    
    
    #
    # Stop the process
    #
    observeEvent(input$stop, {
      rVal$result <- dfEmpty
      if (!is.null(rVal$process)) {
        tools::pskill(rVal$process$pid)
        rVal$msg <- sprintf("%1$s killed", rVal$process$pid)
        rVal$process <- NULL
        
        if (!is.null(rVal$obs)) {
          rVal$obs$destroy()
        }
      }
    })
    
    #
    # Handle process event
    #
    observeEvent(rVal$process, {
      rVal$obs <- observe({
        invalidateLater(500, session)
        isolate({
          result <- mccollect(rVal$process, wait = FALSE)
          if (!is.null(result)) {
            rVal$result <- result
            rVal$obs$destroy()
            rVal$process <- NULL
          }
        })
      })
    })
  }
)