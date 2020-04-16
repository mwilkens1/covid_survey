library(shiny)
library(plotly)
library(ggplot2)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)

#setwd(paste0(getwd(),"/app_web"))

# This is the full dataset that is loaded into the server
load("data/ds.Rda")

ds$w <- 1 

# This is a list of 3 lists of variable names and labels
load("data/varnames.rda")

# List of all categories of each factor variable
load("data/levels_list.rda")
factors <- names(levels_list)

#Creating a list of named breakdowns
breakdown_list <- list("Country" = "B001",
                       "Gender" = "B002",
                       "Age" = "age_group",
                       "Employment status" = "emp_stat",
                       "Education" = "F004"
                       #Add household type?
                       )

#Some style elements
tabpanel_style <- "padding:20px;"

# Define UI 
ui <- fillPage(#The app fills the entire page. It will be iframed into the website
        
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

        sidebarLayout(#layout with a sidebar and main panel
            
        sidebarPanel(width=2,#sidebar has 2/12th of full width
                     
             # Breakdown widget
             h4("Show by"),
             
             pickerInput(inputId = "breakdown", 
                         choices = breakdown_list,
                         selected = "Country",
                         width = "100%"
             ),
             
             br(),
             
             # Filter widgets ----
             h4("Filter data"),
             
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
             
             prettyRadioButtons(
                 inputId = "education_filter",
                 label = "Education", 
                 fill = TRUE,
                 choices = c("All",levels(ds$F004)),
                 selected = "All"
             ),
             
             pickerInput(inputId = "country_filter", label = "Country", 
                         choices = c("EU27", levels(ds$B001)),
                         selected = "EU27",
                         options = list(`live-search` = TRUE),
                         width = "100%"
             ),
             
             pickerInput(inputId = "empstat_filter", label = "Employment status", 
                         choices = c("All", levels(ds$emp_stat)),
                         selected = "All",
                         width = "100%"
             )
        
        ),

        # Main panel next to the sidebar
        mainPanel(width=10,#sidebar has 10/12th of full width
            
            tabsetPanel(#this starts the set of tabs
                
                #First tab
                tabPanel(title="Quality of life", style=tabpanel_style,
                         
                         fluidRow(
                             
                             #Question selection
                             column(width=8,
                             pickerInput(inputId = "var_qol", label = "Select question", 
                                         choices = varnames["Quality of life"],
                                         options = list(`live-search` = TRUE),
                                         width = "100%")),
                             
                             column(width=4,
                             #This dropdown only shows if its a factor variable. The user is supposed to
                             #select a category belonging to the variable selected. 
                             conditionalPanel(
                                 #Here is where the javascript variable comes in. It checks whether the
                                 #selected variable (input 'var_qol') is in the javascript array. If so,
                                 #the extra dropdown is visible.
                                 condition = "input.var_qol && factors.indexOf(input.var_qol) > -1",
                                 #The extra widget is actually created serverside because it needs the 
                                 #selected question as an input variable. See first part of the server.
                                 uiOutput('cat_selector_qol')
                             ))
                         ),
                         
                         fluidRow(
                             #Title of the variable
                             h2(textOutput("title_qol")),

                             #Plot - withspinner from shinycssloaders
                             plotlyOutput("plot_qol") %>% withSpinner()
                             
                         ),
                         
                ),
                
                #second tab
                tabPanel(title="Work and teleworking", style=tabpanel_style,
                         
                         fluidRow(
                             
                             #Question selection
                             column(width=8,
                                    pickerInput(inputId = "var_work", label = "Select question", 
                                                choices = varnames["Work and teleworking"],
                                                options = list(`live-search` = TRUE),
                                                width = "100%")),
                             
                             column(width=4,
                                    conditionalPanel(
                                        condition = "input.var_work && factors.indexOf(input.var_work) > -1",
                                        uiOutput('cat_selector_work')
                                    ))
                         ),
                         
                         fluidRow(
                             
                             #Title of the variable
                             h2(textOutput("title_work")),
                             
                             #Plot - withspinner from shinycssloaders
                             plotlyOutput("plot_work") %>% withSpinner()
                             
                         )
                         
                ),
                
                #third tab                 
                tabPanel(title="Financial situation", style=tabpanel_style,
                         
                         fluidRow(
                             
                             #Question selection
                             column(width=8,
                                    pickerInput(inputId = "var_fin", label = "Select question", 
                                                choices = varnames["Financial situation"],
                                                options = list(`live-search` = TRUE),
                                                width = "100%")),
                             
                             column(width=4,
                                    conditionalPanel(
                                        condition = "input.var_fin && factors.indexOf(input.var_fin) > -1",
                                        uiOutput('cat_selector_fin')
                                    ))
                         ),
                         
                         fluidRow(
                             
                             #Title of the variable
                             h2(textOutput("title_fin")),

                             #Plot - withspinner from shinycssloaders
                             plotlyOutput("plot_fin") %>% withSpinner()
                             
                         )
                )
            )  
        )
    )
)

# Define server 
server <- function(input, output) {

    # This function takes the selected variable as an input and creates 
    # a dropdown widget for selecting the category of that factor variable
    make_cat_selector <- function(inputId, inputvar) {
        
        pickerInput(inputId = inputId, label = "Select category", 
                    choices = levels_list[[inputvar]],
                    width = "100%")
        
    } 
    
    #Applying the function to each tab
    output$cat_selector_qol  <- renderUI(make_cat_selector("cat_sel_qol", input$var_qol)) 
    output$cat_selector_work <- renderUI(make_cat_selector("cat_sel_work",input$var_work)) 
    output$cat_selector_fin  <- renderUI(make_cat_selector("cat_sel_fin", input$var_fin)) 
    
    # Calculating the data
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
