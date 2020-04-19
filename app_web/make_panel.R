#Function to create a panel (tab) in the main body of the app. Because they are all 
#identical I use a function that I call 3 times. Each tab represents a topic:
#Quality of life, Work and teleworking and Financial situation.
make_panel <- function(panel_title,panel_code) {
  
  tabPanel(title=panel_title, style="padding:10px;",
           
           fluidRow(
             
             #Question selection
             column(width=12,
                    pickerInput(inputId = paste0("var_",panel_code), 
                                label = "Select question", 
                                choices = varnames[panel_title],
                                options = list(`live-search` = TRUE),
                                width = "100%"))
           ),
           
           fluidRow(
             
             column(width=6,
             
               # Breakdown widget
               pickerInput(inputId = paste0("breakdown_",panel_code), 
                           label = "By",
                           choices = breakdown_list,
                           selected = "Country",
                           width = "100%"
                           
               )),
             
             column(width=6,
                    #This dropdown only shows if its a factor variable. 
                    # The user is supposed to select a category belonging 
                    # to the variable selected. 
                    conditionalPanel(
                      #Here is where the javascript variable comes in. 
                      #It checks whether the selected variable 
                      #(input 'var_qol') is in the javascript array. 
                      #If so, the extra dropdown is visible.
                      condition = paste0("input.var_",panel_code,
                                         " && factors.indexOf(input.var_",
                                         panel_code,") > -1"),
                      #The extra widget is actually created serverside 
                      #because it needs the selected question as an 
                      #input variable. See first part of the server.
                      uiOutput(paste0('cat_selector_',panel_code))
                    ))
           ),
           
           fluidRow(
             column(width=12,
                    
                    plotlyOutput(paste0("plot_",panel_code), height="500px") %>% withSpinner(),
                    
                    downloadButton(paste0('downloadData_',panel_code), label = "Download data")
                    
             )
           )
           
  )
  
}
