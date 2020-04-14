library(shiny)
library(plotly)

# This sources a function that will get 1 row of data from the API: the data
# of the respondent who just filled in the survey or who got an email with a
# customised link.
source("get_respondent_data.R")

# This is the full dataset that is loaded into the server
load("data/ds.Rda")

# This is a list of 3 lists of variable names and labels
load("data/varnames.rda")

#Some style elements
tabpanel_style <- "padding:20px;"
benchmark_text_style <- "style='font-size:20px; color:red;'"

############## User interface #############
ui <- fillPage(
                
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 2,
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width=10,
      
      tabsetPanel(
        
        tabPanel(title="Quality of life",style=tabpanel_style,
                 
                 fluidRow(
                   
                   selectInput("var_qol", label = "Select question",
                             choices = varnames["Quality of life"],
                             width = "100%"),
                 
                   plotlyOutput("plot_qol"),
                  
                   uiOutput("text_qol")
                 
                 ),
                 
        ),
        
        tabPanel(title="Work and teleworking", style=tabpanel_style,
                 
                 fluidRow(
                   
                   selectInput("var_work", label = "Select question",
                               choices = varnames["Work and teleworking"],
                               width = "100%"),
                 
                   plotlyOutput("plot_work"),
                   
                   tableOutput("text_work")
                 
                 ),
                 
        ),
                 
        tabPanel(title="Financial situation", style=tabpanel_style,
                    
                 fluidRow(
                   
                   selectInput("var_fin", label = "Select question",
                               choices = varnames["Financial situation"],
                               width = "100%"),

                   plotlyOutput("plot_fin"),
                   
                   tableOutput("text_fin")
                 
                )
        )
      )    
    )
  )
)

############# Server #############
server <- function(input, output, session) {
  
  # This downloads the data from the respondent who has opened the app
  respondent_data <- reactive({
    
    # This gets the parameters from the URL (after the ?)
    query <- parseQueryString(session$clientData$url_search)
    case <- query[["IigtHmB"]]
    
    # If there is a ?IigtHmB parameter, download the data from the API
    if (!is.null(case)) {
    
      data <- get_respondent_data(case)
      
      return(data)
    
    #if no paramter return NULL  
    } else {return(NULL)}
    
  })
  
  #Function for creating the benchmark text
  benchmark_text <- function(inputvar) {
    
    # If there is a parameter added to the URL then print the answer to
    # the selected question in the app
    if (!is.null(respondent_data())) {
      
      paste("Your answer:<b ",benchmark_text_style,">",respondent_data()[[inputvar]],"</b>")
      
      # If not return NULL which results in not showing anything  
    } else {NULL}
    
  }
  
  #Function for creating a plot
  make_plot <- function(inputvar) {
    
    plot_ly(data=ds, x=inputvar)
    
  }
  
  #Calling the benchmark_text function to render the text for each section
  output$text_qol <- renderText(benchmark_text(input$var_qol))
  output$text_work <- renderText(benchmark_text(input$var_work))
  output$text_fin <- renderText(benchmark_text(input$var_fin))
  
  #Calling the make_plot function to render the plot for each section
  output$plot_qol <- renderPlotly(make_plot(input$var_qol))
  output$plot_work <- renderPlotly(make_plot(input$var_work))
  output$plot_fin <- renderPlotly(make_plot(input$var_fin))

}

shinyApp(ui,server)
#runApp(list(ui=ui,server=server),launch.browser = TRUE)
