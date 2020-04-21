library(shiny)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(dplyr)
library(purrr)
library(leaflet)
library(leafem)
library(htmlwidgets)
library(htmltools)

#setwd(paste0(getwd(),"/app_web"))

# This is the full dataset that is loaded into the server
load("data/ds.Rda")

ds$w <- 1 

# This is a list of 3 lists of variable names and labels
load("data/varnames.rda")

# List of all categories of each factor variable
load("data/levels_list.rda")
factors <- names(levels_list)

#List of categories initially selected
load("data/sel_levels_list.rda")

# Load the shapefile
load("data/shp_20.rda")

#Creating a list of named breakdowns
breakdown_list <- list("Country" = "B001",
                       "Gender" = "B002",
                       "Age" = "age_group",
                       "Employment status" = "emp_stat",
                       "Education" = "F004"
                       #Add household type?
                       )

#Loading function that creates a panel (tab) in the main body of the app. Because they are all 
#identical I use a function that I call 3 times. Each tab represents a topic:
#Quality of life, Work and teleworking and Financial situation.
source("make_panel.R", local=TRUE)

#Function that calculates the dataset for the plot
source("make_data.R", local=TRUE)

#Function that creates the plot
source("make_plot.R", local=TRUE)

#Function that creates the map
source("make_map.R", local=TRUE)

#Function that creates the description under the figure
source("make_description.R", local=TRUE)

#EF colour scheme
EF_colours <- list("#0D95D0", "#7DC462", "#E72F52", "#774FA0", "#EFB743", "#D44627")

# Define UI 
ui <- fluidPage(theme = shinytheme("cerulean"), #The app fills the entire page. It will be iframed into the website
        
    # This is a little piece of Javascript required for later. 
    tags$head( # refers to the head of the html document
        tags$script({ # start a javascript section in the html
            
            # Here a javascript array is created with all the names of the 
            # factor variables in the form: ['name1','name2',...]
            vars <- NULL
            for (var in factors) {vars <- paste0(vars,"'",var,"'",',')}
            
            vars <- substr(vars, 1, nchar(vars)-1)
            
            #By pasting we get: 
            # var factors = ['name1','name2',...]
            paste0("var factors = [",vars,"];")
            
        })),
    
     fluidRow(   
         
       column(12,
         
        tabsetPanel(#this starts the set of tabs
            
            #See 'make_panel.R'
            
            #First tab
            make_panel("Quality of life","qol"),
            
            #second tab
            make_panel("Work and teleworking", "work"),
            
            #third tab                 
            make_panel("Financial situation", "fin")
            
        )  
        
       )
     ),
     
     #Section under the plot for filtering data
    
     h3("Filter data"),
     
     fluidRow(
         
         column(3,
         awesomeCheckboxGroup(
             inputId = "gender_filter",
             label = "Gender", 
             choices = levels(ds$B002),
             selected = levels(ds$B002)
         )),
         
         column(2,
         awesomeCheckboxGroup(
             inputId = "age_filter",
             label = "Age", 
             choices = levels(ds$age_group),
             selected = levels(ds$age_group)
         )),
         
         column(2,
         awesomeCheckboxGroup(
             inputId = "education_filter",
             label = "Education", 
             choices = levels(ds$F004),
             selected = levels(ds$F004)
         )),
         
         column(5,
         pickerInput(inputId = "country_filter", label = "Country", 
                     choices = levels(ds$B001),
                     selected = levels(ds$B001)[1:27],
                     options = list(`live-search` = TRUE,
                                    `actions-box` = TRUE),
                     multiple=TRUE,
                     width = "100%"
         ),
         
         pickerInput(inputId = "empstat_filter", label = "Employment status", 
                     choices = levels(ds$emp_stat),
                     selected = levels(ds$emp_stat),
                     options = list(`actions-box` = TRUE),
                     multiple = TRUE,
                     width = "100%"
         ))
         
     )

)

# Define server 
server <- function(input, output) {
    
    # This function takes the selected variable as an input and creates 
    # a dropdown widget for selecting the category of that factor variable
    # it is called in the server.
    make_cat_selector <- function(inputId, inputvar) {
        
         pickerInput(inputId = inputId, label = "Select categories", 
                     choices = levels_list[[inputvar]],
                     selected = sel_levels_list[[inputvar]],
                     multiple = TRUE,
                     width = "100%")
               
    } 
    
    #Applying the category selector function to each tab
    output$cat_selector_qol  <- renderUI(make_cat_selector("cat_sel_qol", input$var_qol)) 
    output$cat_selector_work <- renderUI(make_cat_selector("cat_sel_work",input$var_work)) 
    output$cat_selector_fin  <- renderUI(make_cat_selector("cat_sel_fin", input$var_fin)) 

    # Here the make_data function is called ('make_data.R') for each panel
    # It returns a list of a dataframe and a data class (numeric or factor)
    # These variables are used for the plot as well as for the download data function
    data_qol <- reactive(make_data(input$var_qol, input$breakdown_qol, input$cat_sel_qol, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter))
    
    data_work <- reactive(make_data(input$var_work, input$breakdown_work, input$cat_sel_work, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter))
    
    data_fin <- reactive(make_data(input$var_fin, input$breakdown_fin, input$cat_sel_fin, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter))
    
    #Calling the make_plot and make_map function for each tab
    #Both functions are in a seperate R file
    output$map_qol  <- renderLeaflet(make_map(input$var_qol, input$cat_sel_qol, data_qol()))
    output$plot_qol <- renderPlotly(make_plot(input$var_qol, input$cat_sel_qol, data_qol()))
    output$map_work  <- renderLeaflet(make_map(input$var_work, input$cat_sel_work, data_work()))
    output$plot_work <- renderPlotly(make_plot(input$var_work, input$cat_sel_work, data_work()))
    output$map_fin  <- renderLeaflet(make_map(input$var_fin, input$cat_sel_fin, data_fin()))
    output$plot_fin <- renderPlotly(make_plot(input$var_fin, input$cat_sel_fin, data_fin()))
    
    # This function writes the ui. Had to do this server side because it needs to choose
    # between a leafletoutput and a plotly output
    make_plot_ui <- function(breakdown, chart_type, mapoutput, plotoutput) {    
        
        #If breakdown is country and chart type is map
        if (breakdown=="B001" &
            chart_type=="Map") {
            
            #Make the map
            leafletOutput(mapoutput, height="600px") %>% withSpinner()
        
        # in all other cases make the plotly with different heights
        # depending on the breakdown used
        } else {
            
            if (breakdown=="B001") {
                
                height <- "500px"
                
            } else if (breakdown!="emp_stat") {
                
                height <- "300px"
                
            } else {
                
                height <- "400px"
                
            }
            
            plotlyOutput(plotoutput, height=height) %>% withSpinner()
            
        }
        
    }
    
    #Calling the plot ui functions for each tab
    output$plot_ui_qol <-  renderUI(make_plot_ui(input$breakdown_qol,  
                                                 input$chart_type_qol ,"map_qol","plot_qol"))
    output$plot_ui_work <- renderUI(make_plot_ui(input$breakdown_work, 
                                                 input$chart_type_work,"map_work","plot_work"))    
    output$plot_ui_fin <-  renderUI(make_plot_ui(input$breakdown_fin,  
                                                 input$chart_type_fin ,"map_fin","plot_fin"))
    
    #Applying the make discription function to each tab
    output$description_qol <- renderText(make_description(input$cat_sel_qol, input$var_qol))
    output$description_work <- renderText(make_description(input$cat_sel_work, input$var_work))
    output$description_fin <- renderText(make_description(input$cat_sel_fin, input$var_fin))
    
    # Removing commas from the data for the download data function
    data_qol_nocommas <- reactive({
        df <- data_qol()[[1]]
        colnames(df) <- gsub(",", "_", colnames(df))
        return(df)
        })
    data_work_nocommas <- reactive({
        df <- data_work()[[1]]
        colnames(df) <- gsub(",", "_", colnames(df))
        return(df)
    })
    data_fin_nocommas <- reactive({
        df <- data_fin()[[1]]
        colnames(df) <- gsub(",", "_", colnames(df))
        return(df)
    })
    
    # Function for downloading the data
    downloaddata <- function(data) {

        downloadHandler(
            filename = 'EF_data.csv',
            content = function(con) {
                write.csv(data, con)
            }
        )
    }
    
    # Outputs for the download data button
    output$downloadData_qol <- downloaddata(data_qol_nocommas())
    output$downloadData_work <- downloaddata(data_work_nocommas())
    output$downloadData_fin <- downloaddata(data_fin_nocommas())

}

# Run the application 
shinyApp(ui = ui, server = server)