library(shiny)
library(plotly)
library(ggplot2)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(dplyr)

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

#Loading function that creates a panel (tab) in the main body of the app. Because they are all 
#identical I use a function that I call 3 times. Each tab represents a topic:
#Quality of life, Work and teleworking and Financial situation.
source("make_panel.R", local=TRUE)

#Function that calculates the dataset for the plot
source("make_data.R", local=TRUE)

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
        
    sidebarPanel(width=3, #sidebar has x/12th of full width 
                 
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
                     choices = c("All","EU27", levels(ds$B001)),
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
    mainPanel(width=9,#sidebar has 10/12th of full width
        
        tabsetPanel(#this starts the set of tabs
            
            #First tab
            make_panel("Quality of life","qol","printer_qol"),
            
            #second tab
            make_panel("Work and teleworking", "work","printer_work"),
            
            #third tab                 
            make_panel("Financial situation", "fin","printer_fin")
            
        )  
    )
)
)

# Define server 
server <- function(input, output) {
    

    
    # This function takes the selected variable as an input and creates 
    # a dropdown widget for selecting the category of that factor variable
    # it is called in the server.
    make_cat_selector <- function(inputId, inputvar) {
        
         pickerInput(inputId = inputId, label = "Select category", 
                     choices = levels_list[[inputvar]],
                     width = "100%")
 
               
    } 
    
    #Applying the category selector function to each tab
    output$cat_selector_qol  <- renderUI(make_cat_selector("cat_sel_qol", input$var_qol)) 
    output$cat_selector_work <- renderUI(make_cat_selector("cat_sel_work",input$var_work)) 
    output$cat_selector_fin  <- renderUI(make_cat_selector("cat_sel_fin", input$var_fin)) 
    
    data_qol <- reactive(make_data(input$var_qol, input$breakdown, input$cat_sel_qol, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter))
    
    data_work <- reactive(make_data(input$var_work, input$breakdown, input$cat_sel_work, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter))
    
    data_fin <- reactive(make_data(input$var_fin, input$breakdown, input$cat_sel_fin, 
                                   input$gender_filter, input$age_filter, input$education_filter, 
                                   input$country_filter, input$empstat_filter))
    
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

    #Function for making the plot
    make_plot <- function(data) {
        
        class <- data[[2]]
        data <- data[[1]]
        
        x_label <- colnames(data)[1]
        colnames(data)[1] <- "x"
        
        if (class=="numeric") {
            y_label <- "Mean"    
        } else {
            y_label <- "%"
        }
        
        p <- ggplot(data, aes(x=x, y=Mean,text = round(Mean,1))) +
            #Type is bar. Using Eurofound color
            geom_bar(stat="identity", fill="#0D95D0") +
            #removing white space on the y axis
            scale_y_continuous(expand = c(0,0)) + 
            #Setting axis labels
            ylab(y_label) + xlab(NULL) +
            #Horizontal orientation
            coord_flip() +
            #Applying a theme
            theme_bw() +
            #Removing some elements
            theme(panel.border = element_blank(), #no border around the plot
                  panel.grid.major.y = element_blank(), #no vertical gridlines
                  axis.ticks = element_blank()) #no ticks
        
        ggplotly(p, tooltip="text")
        
    }
    
    
    output$plot_qol <- renderPlotly(make_plot(data_qol()))
    output$plot_work <- renderPlotly(make_plot(data_work()))
    output$plot_fin <- renderPlotly(make_plot(data_fin()))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
