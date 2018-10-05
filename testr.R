library(shiny)
  
  ui <- fluidPage(
    p("The checkbox group controls the select input"),
    checkboxGroupInput("inCheckboxGroup", "Input checkbox",
                       c("Item A", "Item B", "Item C")),
    selectInput("inSelect", "Select input",
                c("Item A", "Item B", "Item C")),
    dataTableOutput('dt')
  )
  
  server <- function(input, output, session) {
    output$dt <- renderDataTable({
      data = data.frame(cbind(c(1:10),c(21:30)))
      datatable(data)
    })
    
    observeEvent(input$dt_cell_clicked,{
      print(c("click",input$dt_rows_selected))
    })

    observe({
      input$dt_rows_selected
      print(c("row",input$dt_rows_selected))
    })
    
    observe({
      x <- input$inCheckboxGroup
      
      # Can use character(0) to remove all choices
      if (is.null(x))
        x <- character(0)
      
      # Can also set the label and select items
      updateSelectInput(session, "inSelect",
                        label = paste("Select input label", length(x)),
                        choices = x,
                        selected = tail(x, 1)
      )
    })
  }
  
  shinyApp(ui, server)
