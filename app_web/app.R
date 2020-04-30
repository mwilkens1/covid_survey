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

#Minimun number of cases for a category to be shown
threshold <- 100
threshold_flag <- 200

# This is the full dataset that is loaded into the server
# This is the version of 28 April
load("data/ds_2804.Rda")

#renaming the weight variable
ds <- ds %>% 
      select(-w, -w_gross, -w_trimmed) %>%
      rename(w = w_gross_trim)

# Loading the list with al the variable info
load("data/varinfo.rda")

# Creating vector of the names of factor variables
factors <- names(list.filter(varinfo, "factor" %in% class))

breakdown_list <- list("Country" = "B001",
                       "Gender" = "B002",
                       "Age" = "age_group",
                       "Employment status" = "emp_stat",
                       "Education" = "F004")

# Load the shapefile
load("data/shp_20.rda")

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


subtexts <- lapply(sections, function(s) {
  
  subtexts <- list.filter(varinfo, section==s) %>%  list.mapv(subtext, use.names = FALSE)
  
  names <- as.list(names(list.filter(varinfo, section==s)))
  
  names(names) <- subtexts
  
  return(names)
  
})

names(subtexts) <- sections

# 
# #Getting the subtexts of the relevant variables
# all_variables <- lapply(sections, function(s) {
#    
#    list.filter(varinfo, section==s) %>%  list.mapv(label, use.names = FALSE)
#    
#  }) %>%  unlist() 
#  
# subtexts <- list.filter(varinfo, label %in% all_variables) %>%
#    list.mapv(subtext,use.names = FALSE)
# 

get_subtexts <- function(sections) {
  
    lapply(sections, function(s) {
      
      list.filter(varinfo, section==s) %>%  list.mapv(subtext, use.names = FALSE)
      
  }) %>%  unlist() 

}


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
                       choicesOpt = list(subtext = get_subtexts(sections)),
                       options = list(size = 25),
                       width = "100%")
        )
      ),
      
      fluidRow(
        
        column(width=3,
               
               # Breakdown widget
               pickerInput(inputId = "breakdown", 
                           label = "By",
                           choices = breakdown_list[1:3],
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
                 condition = "input.breakdown == 'B001'",
                 pickerInput(inputId = "chart_type", label = "Chart type", 
                             choices = c("Map","Bar"),
                             selected = "Map",
                             width = "100%")
               )
               
        )
        
      ),
      
      fluidRow(
        column(width=12,
               
               p(htmlOutput("text_above")),
               
               #This shows the plot or map (see server)  
               uiOutput('plot_ui')
               
        )
      ),
      
      fluidRow(style="margin-top: 10px",
               
               column(width=8,
                      
                      #Description under the plot
                      textOutput("text_below")
                      
               ),
               
               column(width=4, align="right",
                      
                      #Download button and link button 
                      div(style="display:inline-block",uiOutput("downloadbutton"),width=6),
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
                            options = list(`actions-box` = TRUE,
                                           title = "Select at least one country"),
                            multiple=TRUE,
                            width = "100%"),

                hidden(
                pickerInput(inputId = "empstat_filter", label = "Employment status", 
                            choices = levels(ds$emp_stat),
                            selected = levels(ds$emp_stat),
                            options = list(`actions-box` = TRUE,
                                           title = "Select at least one employment status"),
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
                 selected = levels(ds$age_group)
                 ),
    
             hidden(
             awesomeCheckboxGroup(
                 inputId = "education_filter",
                 label = "Education", 
                 choices = levels(ds$F004),
                 selected = levels(ds$F004))
             )
            
           ),
           
           a(target="_blank", href="https://github.com/mwilkens1/covid_survey/",
             img(style="position: absolute; right: 0px; bottom: 0px",
                 src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png",
                 align="right",
                 height=25))
          )
      )

)


# Define server 
server <- function(input, output, session) {
  
    # Querying the URL parameters: these are generated when a user clicks 'copy link'
    # Also, a paramater is inbedded on the page on which the shiny app is displayed 
    # on the Eurofound website via an iframe (see below).
    # The parameters can set the inputs so that the same chart with the same inputs
    # can be reproduced
    query <- reactive(parseQueryString(session$clientData$url_search))

    # Creating te dropdown for selecting categories.
    # This dropdown only shows if its a factor variable. 
    # The user is supposed to select a category belonging 
    # to the variable selected and therefore only the categories beloning to 
    # that particular question should be shown. 
    output$cat_selector <-  renderUI({
      
      #First, check if any paramters have been defined in the URL because
      #they should supersede anything else.
      if (!is.null(query()[["cat_sel"]])) {
        
        # Get it and get rid of '_'.
        cat_sel <- query()[["cat_sel"]] %>%
          strsplit("_")
        
        #Selected categories are the ones in the URL parameters
        selected <- cat_sel[[1]]
        
        #If not get the default preferred levels    
      } else {
        
        selected <- varinfo[[input$var]]$default_levels
        
      }
      
      # Only do this if the selected variable is a factor variable
      # 'factors' is defined
      if (input$var %in% factors) {
      
        pickerInput(inputId = "cat_sel", 
                    label = "Select category", 
                    choices = varinfo[[input$var]]$levels,
                    selected = selected,
                    multiple = TRUE,
                    width = "100%",
                    options = list(title = "Select at least one category")) 
        }
      
    })
  
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
    cats_updated <- reactiveVal(FALSE)
    #In case of a change in the selected categories, flag the variable to TRUE
    observeEvent(input$cat_sel, cats_updated(TRUE))
    #In case of an input variable change...
    observeEvent(input$var, {
      
      #... and if the variable is numeric ...
      if (("numeric" %in% varinfo[[input$var]]$class) |
          # ... or at least one of the selected categories are the same
          # as the categories of the last variable selected ...
          sum(input$cat_sel %in% varinfo[[input$var]]$levels)>0 ) 
      # Set updated variable to true, and if not keep it to false
      {cats_updated(TRUE)} else {cats_updated(FALSE)}}
      #In the second round the condition will apply and it will be set to TRUE
      
    )
    
    #Reactive value to show that the data has been updated. 
    #If the data has not been updated the dowload button and the copy link button should
    #not appear.
    data_updated <- reactiveVal(FALSE)
    
    # Here the make_data function is called ('make_data.R') for each panel
    # It returns a list of a dataframe and a data class (numeric or factor)
    # These variables are used for the plot as well as for the download data function
    data <- reactive({

      #Requires the the category selection has been updated
      req(cats_updated())
      #Sets the data updated variable to false
      data_updated(FALSE)
      #Runs the make data function
      data <- make_data(input$var, input$breakdown, input$cat_sel, 
                           input$gender_filter, input$age_filter, input$education_filter, 
                           input$country_filter, input$empstat_filter, threshold)
      #Sets the data updated to TRUE. This step does not occur if one of the conditions
      #specified in the make_data function are not met (see validates in make_data)
      data_updated(TRUE)
    
      return(data)
      
      })
   
    # Calling the make_plot and make_map function for each tab
    # Both functions are in a seperate R file
    output$map  <- renderLeaflet({
      
      #Also require the categories to be updated
      req(cats_updated())
      #Call the make_map function with the data created
      make_map(data())
      
    })
    
    output$plot <- renderPlotly({
      
      req(cats_updated())                                  
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
    output$text_above <- renderText({
      
      #Only appears if there is data
      validate(need(data_updated(), message=""))
      
      paste(make_description(input$cat_sel, input$var), 
            make_filter_description(input$country_filter, input$gender_filter, 
                                    input$age_filter, input$empstat_filter,
                                    input$education_filter),
            make_question_description(input$var),
            varinfo[[input$var]]$extra_text
            )
      
      })
  
    output$text_below <- renderText({
      
      #Only appears if there is data
      validate(need(data_updated(), message=""))
      
      paste(make_excluded_text(data()),
            make_low_reliability_description(data()))
      
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

    #Creating the UI for the downloadbutton. 
    output$downloadbutton <- renderUI({
      
      #Only show it if there is data
      validate(
        need(data_updated(), message="")
      )
      
      downloadButton('downloadData', label = "Download data")
      
      
      })
    
    #Creating the UI for the copy link button
    output$clip <- renderUI({
      
      #Only show it if there is data
      validate(
        need(data_updated(), message="")
      )
      
      rclipButton("clipbtn", "Copy link", make_url(), icon("clipboard"))
      
      })
    
    #Message that link has been copied
    #Observe event ensures the message is shown if the button has been clicked.
    observeEvent(input$clipbtn, {
      showNotification("Link copied to clipboard",type="warning")
    })
    
    
    # What this part also does is it reads the parameters from the url, e.g. ?tab=qol
    # paramaters added to the url overrule any selections made in the app and 
    # automatically adjust them. This functionality is added so that links can be made
    # to specific plot configurations.
    
    # Its triggered only by a change in the URL
    observeEvent(session$clientData$url_search, {
    
      query <- query()  
    
      #Enables extra filters and breakdowns
      if (!is.null(query[["unhide"]])) {
        
        if (query[["unhide"]]=="true") {
             
             show("education_filter")
             show("empstat_filter")
             
             updatePickerInput(session,inputId="breakdown",
                               choices = breakdown_list)
        
         }
      }
      
      # Limit the number of variables shown to those in the section 
      # if section parameter is specified
      if (!is.null(query[["section"]])) {
        
        choices_var <- variables[[query[["section"]]]]
        subtexts <-    get_subtexts(query[["section"]])
        
      } else {
        
        choices_var <- variables
        subtexts <- get_subtexts(sections)
        
      }
      
      #Update other fields according to parameters in the URL
       updatePickerInput(session, "var", selected = query[["var"]],
                                         choices = choices_var,
                                         choicesOpt = list(subtext = subtexts))
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
    paste_parameters <- function(inputvar,inputvar_label,
                                 cat_sel,cat_sel_label,
                                 breakdown,breakdown_label,
                                 chart_type,chart_type_label) {
      
      paste0(
        
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
      
      #Getting the parent URL, because the shiny app is iframed
      runjs('var currentUrl = document.referrer; Shiny.onInputChange("currentUrl",currentUrl);')

      # Question selection, category selection, breakdown selection and chart type
      # are dependent on the tab so parameters are made for each of tab

      # Quality of life tab
      parameters <- paste_parameters(input$var,"var",
                                     input$cat_sel,"cat_sel",
                                     input$breakdown,"breakdown",
                                     input$chart_type,"chart_type")
        
      #Remove the first &
      parameters <- substr(parameters,2,nchar(parameters))
      
      #Pasting the pieces together to one URL
      url <- paste0(sub("\\?.*","\\",input$currentUrl),#this removes any parameters already there
                    "?",
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

}

# Run the application 
shinyApp(ui = ui, server = server)

