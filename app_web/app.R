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

#Creating lists of variable names and labels for the question selection
sections <- c("Quality of life","Work and teleworking","Financial situation")

variables <- lapply(sections, function(s) {

  labels <- list.filter(varinfo, section==s) %>%  list.mapv(label, use.names = FALSE)
  
  names <- as.list(names(list.filter(varinfo, section==s)))
  
  names(names) <- labels
  
  return(names)
  
})

names(variables) <- sections

# Define UI 
ui <- fluidPage(
      title = "Eurofound Living, working and COVID-19 survey data visualisation",
      theme = shinytheme("cerulean"), #The app fills the entire page. It will be iframed into the website
      
      # Activate shiny javascript
      useShinyjs(),

      tags$head( # refers to the head of the html document
        #This CSS code is required to change the position and
        #formatting of the messsage box you get when you click
        #Also it makes sure that the variable selection is on top of the leaflet legend
        #'copy link'. 
        tags$style(
          HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             right: calc(0%);
             background-color: #00b462;
             color: #ffffff;
          }
          
           .leaflet-top, .leaflet-bottom {
             z-index: unset !important;
           }
           
           .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar .leaflet-control-zoom {
             z-index: 10000000000 !important;
           }
          ")
        )),                
                
     #necessary for the clipboard button
     rclipboardSetup(),
     
     fluidRow(
        
        #Question selection
        column(width=12,
               
           pickerInput(inputId = "var", 
                       label = "Select question", 
                       choices = variables,
                       options = list(`live-search` = TRUE,  
                                      size = 25),
                       width = "100%")
           
               
        )
      ),
      
      fluidRow(
        
        column(width=3,
               
               # Breakdown widget
               pickerInput(inputId = "breakdown", 
                           label = "By",
                           choices = breakdown_list,
                           width = "100%")
                           
               ),
        
        column(width=6,
               
               # This dropdown only shows if its a factor variable. 
               # The user is supposed to select a category belonging 
               # to the variable selected. 
               uiOutput("cat_selector")
               
        ),
        
        column(width=3,
               
               #Conditional panel that lets you choose map or bar
               #It only shows up if country is selected as breakdown
               conditionalPanel(
                 condition =  "input.breakdown == 'B001'"),
                 pickerInput(inputId = "chart_type", label = "Chart type", 
                             choices = c("Map","Bar"),
                             selected = "Map",
                             width = "100%")
               )
               
      ),
      
      fluidRow(
        column(width=12,
               
               #This shows the plot or map (see server)  
               uiOutput('plot_ui')
               
        )
      ),
      
      fluidRow(style="margin-top: 10px",
               
               column(width=8,
                      
                      #Description under the plot
                      textOutput("description")
                      
               ),
               
               column(width=4, align="right",
                      
                      #Download button and link button 
                      div(style="display:inline-block",downloadButton('downloadData', label = "Download data"),width=6),
                      div(style="display:inline-block",uiOutput("clip"))
                      
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
                            width = "100%"),
                
                
                hidden(
                pickerInput(inputId = "empstat_filter", label = "Employment status", 
                            choices = levels(ds$emp_stat),
                            selected = levels(ds$emp_stat),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE,
                            width = "100%")
                )
         ),
         
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
  
  
    output$varselector <- renderUI({
      

    })
  
    # Creating te dropdown for selecting categories.
    # This dropdown only shows if its a factor variable. 
    # The user is supposed to select a category belonging 
    # to the variable selected. 
    make_cat_selector <- function(var) {
    
      if (var %in% factors) {
      
        pickerInput(inputId = "cat_sel", 
                    label = "Select category", 
                    choices = varinfo[[var]]$levels,
                    selected = varinfo[[var]]$default_levels,
                    multiple = TRUE,
                    width = "100%") 
        
      }
      
    }

    output$cat_selector <-  renderUI(make_cat_selector(input$var))
  
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
    updated <- reactiveVal(FALSE)
    #In case of a change in the selected categories, flag the variable to TRUE
    observeEvent(input$cat_sel, updated(TRUE))
    #In case of an input variable change...
    observeEvent(input$var, {
      
      #... and if the variable is numeric ...
      if (("numeric" %in% varinfo[[input$var]]$class) |
          # ... or at least one of the selected categories are the same
          # as the categories of the last variable selected ...
          sum(input$cat_sel %in% varinfo[[input$var]]$levels)>0 ) 
      # Set updated variable to true, and if not keep it to false
      {updated(TRUE)} else {updated(FALSE)}}
      #In the second round the condition will apply and it will be set to TRUE
      
    )
    
    
    # Here the make_data function is called ('make_data.R') for each panel
    # It returns a list of a dataframe and a data class (numeric or factor)
    # These variables are used for the plot as well as for the download data function
    data <- reactive({

      req(updated())

      make_data(input$var, input$breakdown, input$cat_sel, 
                           input$gender_filter, input$age_filter, input$education_filter, 
                           input$country_filter, input$empstat_filter, threshold)
      
      })
    

    # Calling the make_plot and make_map function for each tab
    # Both functions are in a seperate R file
    output$map  <- renderLeaflet({
      
      req(updated())
      make_map(data())
      
    })
    
    output$plot <- renderPlotly({
      
      req(updated())                                  
      make_plot(data())
      
     })
    
    
    # This function writes a ui part. Had to do this server side because it needs to choose
    # between a leafletoutput and a plotly output
    make_plot_ui <- function(breakdown, chart_type, mapoutput, plotoutput) {    
        
        #If breakdown is country and chart type is map
        if (breakdown=="B001" &
            chart_type=="Map") {
            
            #Make the map
            leafletOutput(mapoutput, height="600px")  %>% withSpinner() 
        
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
            plotlyOutput(plotoutput, height=height)  %>% withSpinner() 
            
        }
        
    }
    
    # Calling the plot ui functions for each tab
    # renderUI ensures its result is redered as as ui element.
    output$plot_ui <-  renderUI(make_plot_ui(input$breakdown, input$chart_type ,"map","plot"))
    
    # Applying the make_description and make_excluded_text function to each tab
    # make_description is in 'make_description.R' and creates the little description 
    # of what is shown under the plot. This is rendered as text.
    # For some reason the text doesnt update when switching breakdowns without the observe
    observe({
      
      output$description <- renderText(paste(make_description(input$cat_sel, input$var), 
                                                   make_excluded_text(data())))
    })
    
    # The user has the option to download the data that was used to
    # create the plot. The make_data function that was called above prepares the data. 
    # This is used to create the plot and seperately its used by the download button.
    output$downloadData <-  downloadHandler(
          filename = 'EF_data.csv',
          content = function(con) {
            
            df <- data()[[1]]
            # Removing commas from the data for the download data function
            colnames(df) <- gsub(",", "_", colnames(df))
            
            df <- df %>%
              # And rounding to 0 decimals
              mutate_if(is.numeric,round)
            
            write.csv(df, con)
          }
        )
      
    # What this part also does is it reads the parameters from the url, e.g. ?tab=qol
    # paramaters added to the url overrule any selections made in the app and 
    # automatically adjust them. This functionality is added so that links can be made
    # to specific plot configurations.
    
    #Querying the URL
    query <- reactive(parseQueryString(session$clientData$url_search))
    
    # Its triggered only by a change in the URL
    observeEvent(session$clientData$url_search, {
    
      query <- query()  
      
      #Show only variables for section in the parameter
      if (!is.null(query[["section"]])) {
        
        updatePickerInput(session,"var",
                          choices = variables[[query[["section"]]]])
        
        
      }
      
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
             
             updatePickerInput(session,inputId="breakdown",
                               choices = breakdown_list_full)
        
         }
      }
      
      #Update the inputs by calling the functions
      updatePickerInput(session, inputId="cat_sel",
                        choices = varinfo[[input$var]]$levels,
                        selected = change_category("cat_sel",input$var))

      #Update other fields according to parameters in the URL
      updatePickerInput(session, "var", selected = query[["var"]])
      updatePickerInput(session, "breakdown", selected = query[["breakdown"]])
      updatePickerInput(session, "chart_type", selected = query[["chart_type"]])
    
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
      
      paste0("?",
        {if (!is.null(query()[["section"]])) {
          paste0("section=",query()[["section"]])
        }},
        
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
      parameters <- paste_parameters(input$tab,input$var,"var",
                                     input$cat_sel,"cat_sel",
                                     input$breakdown,"breakdown",
                                     input$chart_type,"chart_type")
      
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
    output$clip <-  renderUI({
      rclipButton("clipbtn", "Copy link", make_url(), icon("clipboard"))
    })
    
    #Message that link has been copied
    #Observe event ensures the message is shown if the button has been clicked.
    observeEvent(input$clipbtn, {
      showNotification("Link copied to clipboard",type="warning")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)