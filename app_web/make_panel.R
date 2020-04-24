#Function to create a panel (tab) in the main body of the app. Because they are all 
#identical I use a function that I call 3 times. Each tab represents a topic:
#Quality of life, Work and teleworking and Financial situation.
make_panel <- function(panel_title,panel_code) {
  
  tabPanel(title=panel_title, value=panel_code, style="padding:10px;",
           
           fluidRow(
             
             #Question selection
             column(width=12,
                      pickerInput(inputId = paste0("var_",panel_code), 
                                 label = "Select question", 
                                 choices = {
                                   
                                   labels <- list.filter(varinfo, section==panel_title) %>%  list.mapv(label, use.names = FALSE)
                                     
                                   names <- as.list(names(list.filter(varinfo, section==panel_title)))
                                     
                                   names(names) <- labels
                                     
                                   choices = names
                                     
                                  } ,
                                  options = list(`live-search` = TRUE),
                                  width = "100%")
                   
                    )
           ),
           
           fluidRow(
             
             column(width=3,
             
               # Breakdown widget
               pickerInput(inputId = paste0("breakdown_",panel_code), 
                           label = "By",
                           choices = breakdown_list,
                           width = "100%"
                           
               )),
           
           column(width=6,
                  
                  # This dropdown only shows if its a factor variable. 
                  # The user is supposed to select a category belonging 
                  # to the variable selected. 
                  uiOutput(paste0("cat_selector_",panel_code))
                  
           ),
           
           column(width=3,
                  
                  #Conditional panel that lets you choose map or bar
                  #It only shows up if country is selected as breakdown
                  conditionalPanel(
                    condition =  paste0("input.breakdown_",panel_code,
                                        " == 'B001'"),
                    pickerInput(inputId = paste0("chart_type_",panel_code), label = "Chart type", 
                                choices = c("Map","Bar"),
                                selected = "Map",
                                width = "100%"))
                    
                  )
           ),
           
           fluidRow(
             column(width=12,
              
                  #This shows the plot or map (see server)  
                  uiOutput(paste0('plot_ui_',panel_code))
                    
             )
           ),
           
           fluidRow(
             
             column(width=8,
                  
                  #Description under the plot
                  textOutput(paste0("description_",panel_code))
                    
             ),
             
             column(width=4, align="right",
                    
                  #Download button and link button 
                  div(style="display:inline-block",downloadButton(paste0('downloadData_',panel_code), label = "Download data"),width=6),
                  div(style="display:inline-block",uiOutput(paste0("clip_",panel_code)))
                  
              )
             
           ),

  )
  
}
