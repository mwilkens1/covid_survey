library(shiny)
library(plotly)
library(ggplot2)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)

#setwd(paste0(getwd(),"/app_benchmark"))

# This sources a function that will get 1 row of data from the API: the data
# of the respondent who just filled in the survey or who got an email with a
# customised link.
source("get_respondent_data.R")

# This is the full dataset that is loaded into the server
load("data/ds.Rda")

ds$w <- 1 

# This is a list of 3 lists of variable names and labels
load("data/varinfo.rda")

#Some style elements
tabpanel_style <- "padding:20px;"
benchmark_text_style <- "style='font-size:20px; color:red;'"

############## User interface #############
ui <- fillPage(theme = shinytheme("cerulean"),
    
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 2, #width= 2 means this area is 2/12 of the total width
      
      # input widgets ----
      prettyRadioButtons(
        inputId = "gender_filter",
        label = "Gender", 
        fill = TRUE,
        choices = c("All","Male","Female"),
        selected = "All"
      ),
      
      prettyRadioButtons(
        inputId = "age_filter",
        label = "Age", 
        fill = TRUE,
        choices = c("All",levels(ds$age_group)),
        selected = "All"
      ),
      
      pickerInput(inputId = "country_filter", label = "Country", 
                  choices = c("EU27", levels(ds$B001)),
                  selected = "EU27",
                  options = list(`live-search` = TRUE),
                  width = "100%")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width=10, #Width=10 means this area is 10/12 of the total width
      
      tabsetPanel(#this starts the set of tabs
        
        #First tab
        tabPanel(title="Quality of life", style=tabpanel_style,
                 
                 fluidRow(
                   
                   #Question selection
                   pickerInput(inputId = "var_qol", label = "Select question", 
                               #This code gets the variable labels from varinfo so they can be the choices
                               choices = {
                               labels <- list.filter(varinfo, section=="Quality of life") %>%  list.mapv(label, use.names = FALSE)
                               
                               names <- as.list(names(list.filter(varinfo, section=="Quality of life")))
                               
                               names(names) <- labels
                               
                               choices = names
                               
                               },
                               
                     #choices = varnames["Quality of life"],
                     options = list(`live-search` = TRUE),
                     width = "100%"),
                  
                   #Title of the variable
                   h2(textOutput("title_qol")),
                   
                   #Benchmark text (if applicable)
                   uiOutput("text_qol"),
                 
                   #Plot - withspinner from shinycssloaders
                   plotlyOutput("plot_qol") %>% withSpinner()
                 
                 ),
                 
        ),
        
        #second tab
        tabPanel(title="Work and teleworking", style=tabpanel_style,
                 
                 fluidRow(
                   
                   #Question selection
                   pickerInput(inputId = "var_work", label = "Select question", 
                               choices = {
                                 labels <- list.filter(varinfo, section=="Work and teleworking") %>%  list.mapv(label, use.names = FALSE)
                                 
                                 names <- as.list(names(list.filter(varinfo, section=="Work and teleworking")))
                                 
                                 names(names) <- labels
                                 
                                 choices = names
                                 
                               },
                               options = list(`live-search` = TRUE),
                               width = "100%"),
                   
                   #Title of the variable
                   h2(textOutput("title_work")),
                   
                   #Benchmark text (if applicable)
                   uiOutput("text_work"),
                 
                   #Plot - withspinner from shinycssloaders
                   plotlyOutput("plot_work") %>% withSpinner()

                 
                 ),
                 
        ),

        #third tab                 
        tabPanel(title="Financial situation", style=tabpanel_style,
                    
                 fluidRow(
                   
                   #Question selection
                   pickerInput(inputId = "var_fin", label = "Select question", 
                               choices = {
                                 labels <- list.filter(varinfo, section=="Financial situation") %>%  list.mapv(label, use.names = FALSE)
                                 
                                 names <- as.list(names(list.filter(varinfo, section=="Financial situation")))
                                 
                                 names(names) <- labels
                                 
                                 choices = names
                                 
                               },
                               options = list(`live-search` = TRUE),
                               width = "100%"),
                   
                   #Title of the variable
                   h2(textOutput("title_fin")),

                   #Benchmark text (if applicable)
                   uiOutput("text_fin"),
                   
                   #Plot - withspinner from shinycssloaders
                   plotlyOutput("plot_fin") %>% withSpinner()
                   
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
  
  #Function for creating the title
  make_title <- function(inputvar) {
    
    #Titles are stored as comments to the question (see import.r)
    comment(ds[[inputvar]])
    
  }
  
  #Function for creating a plot
  make_plot <- function(inputvar, country_filter, gender_filter, age_filter) {
    
    p <- ds %>% #Take the dataset
      select(!!inputvar, w, EU27, B001, B002, age_group) %>% #select input variable and weight
      filter(!is.na(!!sym(inputvar))) %>% # Filter any missing values 
      
      #Filter EU27 only or filter by country selected
      {if (country_filter=="EU27") filter(., EU27 == TRUE) else 
        filter(., B001 == !!country_filter)} %>% 
      
      #Filter by gender
      {if (gender_filter!="All") filter(., B002 == !!gender_filter) else .} %>%
      
      #Filter by age group
      {if (age_filter!="All") filter(., age_group == !!age_filter) else .} %>%
      
      #Mutate to factor: this is needed for some numeric variables that 
      #should be treated as a factor, e.g. the happiness scale
      mutate(!!inputvar := as.factor(!!sym(inputvar))) %>% 
      
      #Pass the data to ggplot
      ggplot(aes(x = !!sym(inputvar), 
                 # this counts the proportions
                 y = (..count..)/sum(..count..) * 100, 
                 # data is weighted with w
                 weight = w,
                 # This generates the hover text
                 text = paste0(round((..count..)/sum(..count..)*100,0),"%"))) + 
      #type is barplot
      geom_bar(fill="#0D95D0") +
      #removing white space on the y axis
      scale_y_continuous(expand = c(0,0)) + 
      #Setting axis labels
      ylab("%") + xlab(NULL) +
      #Horizontal orientation
      coord_flip() +
      #Applying a theme
      theme_bw() +
      #Removing some elements
      theme(panel.border = element_blank(), #no border around the plot
            panel.grid.major.y = element_blank(), #no vertical gridlines
            axis.ticks = element_blank()) #no ticks
    
    #Pass ggplot object to ggplotly to make it interactive
    ggplotly(p, tooltip="text")

  }
  
  #Function for creating the benchmark text
  benchmark_text <- function(inputvar) {
    
    # If there is a parameter added to the URL then print the answer to
    # the selected question in the app
    if (!is.null(respondent_data())) {
      
      #This calls the repondentdata selects the input variable. 
      #This is then wrapped in a paste to create a string.
      #It includes styling: bold and other elements defined in benchmark_text_style
      paste("Your answer:<b ",benchmark_text_style,">",
            respondent_data()[[inputvar]],"</b>")
      
      # If not return NULL which results in not showing anything  
    } else {NULL}
    
  }
  
  #Calling the make_title function to render the title for each section
  output$title_qol <- renderText(make_title(input$var_qol))
  output$title_work <- renderText(make_title(input$var_work))
  output$title_fin <- renderText(make_title(input$var_fin))
  
  #Calling the make_plot function to render the plot for each section
  output$plot_qol <- renderPlotly(make_plot(input$var_qol, 
                                            input$country_filter,
                                            input$gender_filter,
                                            input$age_filter))
  output$plot_work <- renderPlotly(make_plot(input$var_work, 
                                             input$country_filter,
                                             input$gender_filter,
                                             input$age_filter))
  output$plot_fin <- renderPlotly(make_plot(input$var_fin, 
                                            input$country_filter,
                                            input$gender_filter,
                                            input$age_filter))

  #Calling the benchmark_text function to render the text for each section
  output$text_qol <- renderText(benchmark_text(input$var_qol))
  output$text_work <- renderText(benchmark_text(input$var_work))
  output$text_fin <- renderText(benchmark_text(input$var_fin))
  
}

#This runs the shiny app
shinyApp(ui,server)
#runApp(list(ui=ui,server=server),launch.browser = TRUE)