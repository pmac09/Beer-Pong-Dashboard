#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  selectInput("test","test",c(1,2,3))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  reaact <- reactive({
    print(input$test)
    return(input$test)
  })
  
  observe({
    print(input$test)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

