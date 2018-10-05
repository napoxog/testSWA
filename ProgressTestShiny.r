library(shiny)
library(shinyjs)

myPeriodicFunction <- function(){
  for(i in 1:5){
    msg <- paste(sprintf("Step %d done.... \n",i))
    cat(msg)
    Sys.sleep(1)
  }
}

# Override cat function
#cat <- message
options(shiny.reactlog = TRUE)
runApp(shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    actionButton("btn","Click me"),
    textOutput("text")
  ),
  server = function(input,output, session) {
    observeEvent(input$btn, {
      withCallingHandlers({
        shinyjs::html("text", "")
        myPeriodicFunction()
      },
      message = function(m) {
        shinyjs::html(id = "text", html = m$message, add = FALSE)
      })
    })
  }
))