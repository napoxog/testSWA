library(shiny)
library(miniUI)


plotZoom <- function (name,data) {
  ui <- miniPage(
    gadgetTitleBar(name),
    miniContentPanel(
      plotOutput("gadgetPlot", height = "100%")
    )
  )
  server <- function(input, output, session) {
    
    # Render the plot
    output$plot <- renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      ggplot(data, aes_string(xvar, yvar)) + geom_point()
    })
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(brushedPoints(data, input$brush))
    })
  }
  
  runGadget(ui, server)
}

