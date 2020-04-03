library(shiny)

############## User interface #############
ui <- fluidPage(
  
  textOutput("param_text")
  
)

############# Server #############
server <- function(input, output, session) {
  
    # This gets the parameters from the URL (after the ?)
    query <- reactive(parseQueryString(session$clientData$url_search))
  
    output$param_text <- renderText({
      
      query()[["text"]]
      
    })

  }

shinyApp(ui,server)