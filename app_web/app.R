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
library(rclipboard)
library(rlist)

#setwd(paste0(getwd(),"/app_web"))

#URL where the app is iframed
domain <- "https://eurofound.acc.fpfis.tech.ec.europa.eu/data/covid-19-survey-web-visualisation"

#Minimun number of cases for a category to be shown
threshold <- 200

# This is the full dataset that is loaded into the server
load("data/ds.Rda")

ds$w <- 1 

# Loading the list with al the variable info
load("data/varinfo.rda")

# Creating vector of the names of factor variables
factors <- names(list.filter(varinfo, "factor" %in% class))

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
ui <- fluidPage(
      title = "Eurofound Living, working and COVID-19 survey data visualisation",
      theme = shinytheme("cerulean"), #The app fills the entire page. It will be iframed into the website
                
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
          
        }),
        #This CSS code is required to change the position and
        #formatting of the messsage box you get when you click
        #'copy link'. 
        tags$style(
          HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             right: calc(0%);
             background-color: #00b462;
             color: #ffffff;
             }
             "
          )
        )),                
                
     #necessary for the clipboard button
     rclipboardSetup(),
      
     #Here the actual layout of the page starts  
     fluidRow(   
         
       column(12,
         
        tabsetPanel(id="tab",#this starts the set of tabs
            
            #The tabsetpanel set up makes things a little complicated because
            #there are now three different widgets for selecting questions 
            #who all have a different set of questions. So a lot of things server
            #side need to to be run for each panel.
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

         column(5,
                pickerInput(inputId = "country_filter", label = "Country", 
                            # Choices are all the levels of the country factor
                            choices = levels(ds$B001),
                            # Selected are the EU27 by default
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
                )),
         
         column(7,
           #splitlayout evenly spreads the elements within the column    
           splitLayout(
                    
             awesomeRadio(
                 inputId = "gender_filter",
                 label = "Gender", 
                 choices = c("All","Male","Female"),
                 selected = "All"),
    
             awesomeCheckboxGroup(
                 inputId = "age_filter",
                 label = "Age", 
                 choices = levels(ds$age_group),
                 selected = levels(ds$age_group)),
    
             awesomeCheckboxGroup(
                 inputId = "education_filter",
                 label = "Education", 
                 choices = levels(ds$F004),
                 selected = levels(ds$F004))
           )
        )
     )
)





# Define server 
server <- function(input, output, session) {

    # This section updates the categories with the categories relevant for the
    # selected question. Some questions have a 5 point likert scale while others 
    # are yes / no for example. 
    # What this part also does is it reads the parameters from the url, e.g. ?tab=qol
    # paramaters added to the url overrule any selections made in the app and 
    # automatically adjust them. This functionality is added so that links can be made
    # to specific plot configurations.
    
    observe({
        
        #Querying the URL
        query <- parseQueryString(session$clientData$url_search)
        
        #Function for updating the selected categories
        change_category <- function(parameter,inputvar) {
            
            # if a parameter is present in URL
            if (!is.null(query[[parameter]])) {
                
                # Get it and get rid of '_'
                cat_sel <- query[[parameter]] %>%
                    strsplit("_")
                
                selected <- cat_sel[[1]]
                
            #If not get the default preferred levels    
            } else {
                
                selected <- varinfo[[inputvar]]$default_levels
                
            }
        
        }
        
        #Update the inputs by calling the functions
        updatePickerInput(session, inputId="cat_sel_qol",
                          choices = varinfo[[input$var_qol]]$levels,
                selected = change_category("cat_sel_qol",input$var_qol))
    
        updatePickerInput(session, inputId="cat_sel_work",
                          choices = varinfo[[input$var_work]]$levels,
                selected = change_category("cat_sel_work",input$var_work))
    
        updatePickerInput(session, inputId="cat_sel_fin",
                          choices = varinfo[[input$var_fin]]$levels,
                selected = change_category("cat_sel_fin",input$var_fin))
        
        #Update panel choice
        updateTabsetPanel(session,"tab",selected = query[["tab"]])
        
        #Update other fields according to parameters in the URL
        updatePickerInput(session, "var_qol", selected = query[["var_qol"]])
        updatePickerInput(session, "var_work", selected = query[["var_work"]])
        updatePickerInput(session, "var_fin", selected = query[["var_fin"]])
        
        updatePickerInput(session, "breakdown_qol", selected = query[["breakdown_qol"]])
        updatePickerInput(session, "breakdown_work", selected = query[["breakdown_work"]])
        updatePickerInput(session, "breakdown_fin", selected = query[["breakdown_fin"]])
        
        updatePickerInput(session, "chart_type_qol", selected = query[["chart_type_qol"]])
        updatePickerInput(session, "chart_type_work", selected = query[["chart_type_work"]])
        updatePickerInput(session, "chart_type_fin", selected = query[["chart_type_fin"]])
        
        #Update filters. These are not dependent on the tabs
        for (filter in c("country_filter","empstat_filter")) {
                    
            if (!is.null(query[[filter]])) {
                
                #parameter value must come in the form of country_country
                selection <- query[[filter]] %>%
                    strsplit("_")
                
                updatePickerInput(session, filter, selected = selection[[1]] )
            }    
        
        }
        
        for (filter in c("gender_filter","age_filter","education_filter")) {
            
            if (!is.null(query[[filter]])) {    
            
                #parameter value must come in the form of country_country
                selection <- query[[filter]] %>%
                    strsplit("_")
                
                updateAwesomeCheckboxGroup(session, filter, selected = selection[[1]] )
            }    
            
        }

    })
    
    
    
    #This part constructs a URL with parameters that can be copied by the user 
    #to go back to the same exact plot configuration later
    
    #Function to create a parameter string
    make_parameter <- function(input,inputstring) {
        
        if (!is.null(input)) {
            
            paste0("&",inputstring,"=",input)
            
        }
        
    }
    
    # Function to create parameter string with multiple values.
    # it places an underscore between each value.
    # e.g. '&country_filter=Austria_Germany'
    make_multiple_parameter <- function(input, inputstring) {
        
        if (!is.null(input)) {
            
            text <- paste0("&",inputstring,"=")
            
            for (i in input) {
                
                text <- paste0(text,i,"_")
                
            }
          
            #Removing final underscore 
            return(substr(text,1,nchar(text)-1))
            
        }
        
    }
    
    #Putting it together: create URL to reproduce the plot
    make_url <- reactive({
       
      # Question selection, category selection, breakdown selection and chart type
      # are dependent on the tab so parameters are made for each of tab
      # Quality of life tab
      if (input$tab=="qol") {
          
          parameters <- paste0(
              
              "?tab=qol",
              make_parameter(input$var_qol,"var_qol"),
              make_multiple_parameter(input$cat_sel_qol,"cat_sel_qol"),
              make_parameter(input$breakdown_qol,"breakdown_qol"),
              make_parameter(input$chart_type_qol,"chart_type_qol")
          
          )
      
      # Work tab        
      } else if (input$tab=="work") {
          
          parameters <- paste0(
              
              "?tab=work",
              make_parameter(input$var_work,"var_work"),
              make_multiple_parameter(input$cat_sel_work,"cat_sel_work"),
              make_parameter(input$breakdown_work,"breakdown_work"),
              make_parameter(input$chart_type_work,"chart_type_work")
              
          )
      
      #Financial security tab        
      } else if (input$tab=="fin") {
          
          parameters <- paste0(
              
              "?tab=fin",
              make_parameter(input$var_fin,"var_fin"),
              make_multiple_parameter(input$cat_sel_fin,"cat_sel_fin"),
              make_parameter(input$breakdown_fin,"breakdown_fin"),
              make_parameter(input$chart_type_fin,"chart_type_fin")
              
          )
          
      }
      
      #Pasting the pieces together to one URL
      url <- paste0(domain,
                    parameters,
                    make_multiple_parameter(input$country_filter,"country_filter"),
                    make_multiple_parameter(input$empstat_filter,"empstat_filter"),
                    make_multiple_parameter(input$gender_filter,"gender_filter"),
                    make_multiple_parameter(input$age_filter,"age_filter"),
                    make_multiple_parameter(input$education_filter,"education_filter")
              ) %>%
        # Passing it trough URL encode to ensure compatibiltiy in the browser
        URLencode()
      
    })

    
    
    # Here the make_data function is called ('make_data.R') for each panel
    # It returns a list of a dataframe and a data class (numeric or factor)
    # These variables are used for the plot as well as for the download data function

    data_qol <- reactive(make_data(input$var_qol, input$breakdown_qol, input$cat_sel_qol, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter, threshold))
    
    data_work <- reactive(make_data(input$var_work, input$breakdown_work, input$cat_sel_work, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter, threshold))
    
    data_fin <- reactive(make_data(input$var_fin, input$breakdown_fin, input$cat_sel_fin, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter, threshold))

    # Calling the make_plot and make_map function for each tab
    # Both functions are in a seperate R file
    output$map_qol  <- renderLeaflet(make_map(input$var_qol, input$cat_sel_qol, data_qol()))
    output$plot_qol <- renderPlotly(make_plot(input$var_qol, input$cat_sel_qol, data_qol()))
    output$map_work  <- renderLeaflet(make_map(input$var_work, input$cat_sel_work, data_work()))
    output$plot_work <- renderPlotly(make_plot(input$var_work, input$cat_sel_work, data_work()))
    output$map_fin  <- renderLeaflet(make_map(input$var_fin, input$cat_sel_fin, data_fin()))
    output$plot_fin <- renderPlotly(make_plot(input$var_fin, input$cat_sel_fin, data_fin()))
    
    # This function writes a ui part. Had to do this server side because it needs to choose
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
            
            #and make the plotly
            plotlyOutput(plotoutput, height=height) %>% withSpinner()
            
        }
        
    }
    
    # Calling the plot ui functions for each tab
    # renderUI ensures its result is redered as as ui element.
    output$plot_ui_qol <-  renderUI(make_plot_ui(input$breakdown_qol,  
                                                 input$chart_type_qol ,"map_qol","plot_qol"))
    output$plot_ui_work <- renderUI(make_plot_ui(input$breakdown_work, 
                                                 input$chart_type_work,"map_work","plot_work"))    
    output$plot_ui_fin <-  renderUI(make_plot_ui(input$breakdown_fin,  
                                                 input$chart_type_fin ,"map_fin","plot_fin"))
    
    # Applying the make_description and make_excluded_text function to each tab
    # make_description is in 'make_description.R' and creates the little description 
    # of what is shown under the plot. This is rendered as text.
    # For some reason the text doesnt update when switching breakdowns without the observe
    observe({
      
      output$description_qol <- renderText(paste(make_description(input$cat_sel_qol, input$var_qol), 
                                                   make_excluded_text(data_qol())))
      output$description_work <- renderText(paste(make_description(input$cat_sel_work, input$var_work), 
                                                    make_excluded_text(data_work())))
      output$description_fin <- renderText(paste(make_description(input$cat_sel_fin, input$var_fin), 
                                                   make_excluded_text(data_fin())))
    })
    
    # The user has the option to download the data that was used to
    # create the plot. The make_data function that was called above prepares the data. 
    # This is used to create the plot and seperately its used by the download button.
    output$downloadData_qol <-  downloadHandler(
          filename = 'EF_data.csv',
          content = function(con) {
            
            df <- data_qol()[[1]]
            # Removing commas from the data for the download data function
            colnames(df) <- gsub(",", "_", colnames(df))
            
            df <- df %>%
              # And rounding to 0 decimals
              mutate_if(is.numeric,round)
            
            write.csv(df, con)
          }
        )
        
    
    output$downloadData_work <-  downloadHandler(
      filename = 'EF_data.csv',
      content = function(con) {
        
        df <- data_work()[[1]]
        colnames(df) <- gsub(",", "_", colnames(df))
        
        df <- df %>%
          mutate_if(is.numeric,round)
        
        write.csv(df, con)
      }
    )
    
      
    output$downloadData_fin <-  downloadHandler(
      filename = 'EF_data.csv',
      content = function(con) {
        
        df <- data_fin()[[1]]
        colnames(df) <- gsub(",", "_", colnames(df))
        
        df <- df %>%
          mutate_if(is.numeric,round)
        
        write.csv(df, con)
      }
    )
    
    # Here is the button to copy the URL for the plot configuration
    # Three different ones are made for each panel.
    # Add clipboard buttons
    output$clip_qol <- renderUI({
      rclipButton("clipbtn_qol", "Copy link", make_url(), icon("clipboard"))
    })
    
    output$clip_work <- renderUI({
      rclipButton("clipbtn_work", "Copy link", make_url(), icon("clipboard"))
    })
    
    output$clip_fin <- renderUI({
      rclipButton("clipbtn_fin", "Copy link", make_url(), icon("clipboard"))
    })
    
    #Message that link has been copied
    #Observe event ensures the message is shown if the button has been clicked.
    observeEvent(input$clipbtn_qol, {
      showNotification("Link copied to clipboard",type="warning")
    })
    
    observeEvent(input$clipbtn_work, {
      showNotification("Link copied to clipboard",type="warning")
    })
    
    observeEvent(input$clipbtn_fin, {
      showNotification("Link copied to clipboard",type="warning")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
