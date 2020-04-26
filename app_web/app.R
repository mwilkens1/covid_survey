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
library(shinyjs)

#setwd(paste0(getwd(),"/app_web"))

#URL where the app is iframed
domain <- "https://eurofound.acc.fpfis.tech.ec.europa.eu/data/covid-19-survey-web-visualisation"

#Minimun number of cases for a category to be shown
threshold <- 200

# This is the full dataset that is loaded into the server
load("data/ds.Rda")

# Loading the list with al the variable info
load("data/varinfo.rda")

# Creating vector of the names of factor variables
factors <- names(list.filter(varinfo, "factor" %in% class))

#Creating a list of named breakdowns
breakdown_list <- list("Country" = "B001",
                       "Gender" = "B002",
                       "Age" = "age_group")

# Load the shapefile
load("data/shp_20.rda")

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
      
      useShinyjs(),
                
      tags$head( # refers to the head of the html document
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
                
                hidden(
                pickerInput(inputId = "empstat_filter", label = "Employment status", 
                            choices = levels(ds$emp_stat),
                            selected = levels(ds$emp_stat),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE,
                            width = "100%"
                )
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
    
             hidden(
             awesomeCheckboxGroup(
                 inputId = "education_filter",
                 label = "Education", 
                 choices = levels(ds$F004),
                 selected = levels(ds$F004))
             )
           )
        )
     )
)





# Define server 
server <- function(input, output, session) {
    
  
  #  if (is.null(query[["unhide"]])) {
      

      
   # } 
    
  
    # Creating te dropdown for selecting categories.
    # This dropdown only shows if its a factor variable. 
    # The user is supposed to select a category belonging 
    # to the variable selected. 
    make_cat_selector <- function(inputvar, panel_code) {
    
      if (inputvar %in% factors) {
      
        pickerInput(inputId = paste0("cat_sel_",panel_code), 
                    label = "Select category", 
                    choices = varinfo[[inputvar]]$levels,
                    selected = varinfo[[inputvar]]$default_levels,
                    multiple = TRUE,
                    width = "100%") 
        
      }
      
    }

    output$cat_selector_qol <-  renderUI(make_cat_selector(input$var_qol, "qol"))
    output$cat_selector_work <- renderUI(make_cat_selector(input$var_work, "work")) 
    output$cat_selector_fin <-  renderUI(make_cat_selector(input$var_fin, "fin"))

    
    # Because the available categories are dependent on the inputvariable, R shiny does:
    # inputvar change -> run data + plot + update categories -> run data + plot 
    # The first step is triggered by the inputvariable change and the second step
    # by the category change. Therefore we have to stop it from running data and plotting
    # when the catogories have not been updated yet. Note: this only is an issue with
    # categorical data. Running twice doesnt really matter that much because in a split
    # second it is replaced by the second plot but still its nicer to avoid it.
    
    #Reactive value saying that the categories have not been updated
    #This variable is used later on by requiring that it is TRUE for the data and plot
    #functions to be run.
    updated_qol <- reactiveVal(FALSE)
    #In case of a change in the selected categories, flag the variable to TRUE
    observeEvent(input$cat_sel_qol, updated_qol(TRUE))
    #In case of an input variable change...
    observeEvent(input$var_qol, {
      
      #... and if the variable is numeric ...
      if (("numeric" %in% varinfo[[input$var_qol]]$class) |
          # ... or at least one of the selected categories are the same
          # as the categories of the last variable selected ...
          sum(input$cat_sel_qol %in% varinfo[[input$var_qol]]$levels)>0 ) 
      # Set updated variable to true, and if not keep it to false
      {updated_qol(TRUE)} else {updated_qol(FALSE)}}
      #In the second round the condition will apply and it will be set to TRUE
      
    )
    
    #Same system for the other tabs
    updated_work <- reactiveVal(FALSE)
    observeEvent(input$cat_sel_work, updated_work(TRUE))
    observeEvent(input$var_work, {
      
      if (("numeric" %in% varinfo[[input$var_work]]$class) |
          sum(input$cat_sel_work %in% varinfo[[input$var_work]]$levels)>0 ) 
      {updated_work(TRUE)} else {updated_work(FALSE)}}
    )
    
    updated_fin <- reactiveVal(FALSE)
    observeEvent(input$cat_sel_fin, updated_fin(TRUE))
    observeEvent(input$var_fin, {
      
      if (("numeric" %in% varinfo[[input$var_fin]]$class) |
          sum(input$cat_sel_fin %in% varinfo[[input$var_fin]]$levels)>0 ) 
      {updated_fin(TRUE)} else {updated_fin(FALSE)}}
    )

    
    # Here the make_data function is called ('make_data.R') for each panel
    # It returns a list of a dataframe and a data class (numeric or factor)
    # These variables are used for the plot as well as for the download data function
    data_qol <- reactive({

      req(updated_qol())

      make_data(input$var_qol, input$breakdown_qol, input$cat_sel_qol, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter, threshold)
      
      })
    
    data_work <- reactive({
      
      req(updated_work())
      
      make_data(input$var_work, input$breakdown_work, input$cat_sel_work, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter, threshold)
      
      })
    
    data_fin <- reactive({
      
      req(updated_fin())
      make_data(input$var_fin, input$breakdown_fin, input$cat_sel_fin, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter, threshold)
    })

    # Calling the make_plot and make_map function for each tab
    # Both functions are in a seperate R file
    output$map_qol  <- renderLeaflet({
      
      req(updated_qol())
      make_map(data_qol())
      
    })
    
    output$plot_qol <- renderPlotly({
      
      req(updated_qol())                                  
      make_plot(data_qol())
      
     })
    
    output$map_work  <- renderLeaflet({
      
      req(updated_work())
      make_map(data_work())
      
    })
    
    output$plot_work <- renderPlotly({
      
      req(updated_work())
      make_plot(data_work())
      
    })
    
    output$map_fin  <- renderLeaflet({
      
      req(updated_fin())
      make_map(data_fin())
  
    })
    
    output$plot_fin <- renderPlotly({
     
      req(updated_fin()) 
      make_plot(data_fin())
      
    })
    
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
    
    # What this part also does is it reads the parameters from the url, e.g. ?tab=qol
    # paramaters added to the url overrule any selections made in the app and 
    # automatically adjust them. This functionality is added so that links can be made
    # to specific plot configurations.
    
    # Its triggered only by a change in the URL
    observeEvent(session$clientData$url_search, {
      
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
      
      #Enables extra filters and breakdowns
      if (!is.null(query[["unhide"]])) {
        
        if (query[["unhide"]]=="true") {
             
             show("education_filter")
             show("empstat_filter")
             
             breakdown_list_full <- list("Country" = "B001",
                                    "Gender" = "B002",
                                    "Age" = "age_group",
                                    "Employment status" = "emp_stat",
                                    "Education" = "F004")
             
             updatePickerInput(session,inputId="breakdown_qol",
                               choices = breakdown_list_full)
             updatePickerInput(session,inputId="breakdown_work",
                               choices = breakdown_list_full)
             updatePickerInput(session,inputId="breakdown_fin",
                               choices = breakdown_list_full)
             
             
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
    
    #Function for pasting the different sets of parameters together
    #The if statemets are there to prevent a parameter from being 
    #added if the default inputs are chosen. 
    paste_parameters <- function(tab,inputvar,inputvar_label,
                                 cat_sel,cat_sel_label,
                                 breakdown,breakdown_label,
                                 chart_type,chart_type_label) {
      
      paste0(
        
        "?tab=",tab,
        make_parameter(inputvar,inputvar_label),
        make_multiple_parameter(cat_sel,cat_sel_label),
        { if (breakdown!="B001") 
          make_parameter(breakdown,breakdown_label)
        },
        { if (chart_type!="Map")
          
          make_parameter(chart_type,chart_type_label)
          
        }
        
      )
      
    }
    
    
    #Putting it together: create URL to reproduce the plot
    make_url <- reactive({
      
      # Question selection, category selection, breakdown selection and chart type
      # are dependent on the tab so parameters are made for each of tab

      # Quality of life tab
      if (input$tab=="qol") {
        
        parameters <- paste_parameters(input$tab,input$var_qol,"var_qol",
                                      input$cat_sel_qol,"cat_sel_qol",
                                      input$breakdown_qol,"breakdown_qol",
                                      input$chart_type_qol,"chart_type_qol")
        
        # Work tab        
      } else if (input$tab=="work") {
        
        parameters <- paste_parameters(input$tab,input$var_work,"var_work",
                                      input$cat_sel_work,"cat_sel_work",
                                      input$breakdown_work,"breakdown_work",
                                      input$chart_type_work,"chart_type_work")
        
        #Financial security tab        
      } else if (input$tab=="fin") {
        
        parameters <- paste_parameters(input$tab,input$var_fin,"var_fin",
                                      input$cat_sel_fin,"cat_sel_fin",
                                      input$breakdown_fin,"breakdown_fin",
                                      input$chart_type_fin,"chart_type_fin")
        
      }
      
      #Pasting the pieces together to one URL
      url <- paste0(domain,
                    parameters,
                    # If number of selected counties is not the same as the number of
                    # default countries ...
                    { if (length(input$country_filter)!=length(levels(ds$B001)[1:27])) {
                      
                      #... then make the parameter
                      make_multiple_parameter(input$country_filter,"country_filter")
                      
                      # if the length is the same ... 
                      } else {
                        
                        #...but any of the selected countries are not EU27...
                        if (any(input$country_filter!=levels(ds$B001)[1:27]))
                          
                          #... make the parameter
                          make_multiple_parameter(input$country_filter,"country_filter")
                      
                      }
                      
                    },
                    # if the number of categories selected does not equal the number of 
                    # default categories ...
                    { if (length(input$empstat_filter)!=length(levels(ds$emp_stat)))
                    #.. add the parameter
                    make_multiple_parameter(input$empstat_filter,"empstat_filter")
                    },
                    { if (input$gender_filter!="All")
                    make_multiple_parameter(input$gender_filter,"gender_filter")
                    },
                    { if (length(input$age_filter)!=length(levels(ds$age_group)))
                    make_multiple_parameter(input$age_filter,"age_filter")
                    },
                    { if (length(input$education_filter)!=length(levels(ds$F004)))
                    make_multiple_parameter(input$education_filter,"education_filter")
                    }
      ) %>%
        # Passing it trough URL encode to ensure compatibiltiy in the browser
        URLencode()
      
    })
    
    
    # Here is the button to copy the URL for the plot configuration
    # Three different ones are made for each panel.
    # Add clipboard buttons
    output$clip_qol <-  output$clip_work <- output$clip_fin <- 
      renderUI({
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
