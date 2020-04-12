library(shiny)
library(plotly)
library(tidyr)
library(dplyr)

#setwd(paste0(getwd(),"/app_response"))

############## User interface #############
ui <- fluidPage( style = 'width:1200px;',

   h1("Eurofound survey: Living, Working and COVID-19"),
   h4(textOutput("extraction_date")),
   br(),
                 
   fluidRow(
            
    column(3,
      
      h3("Response:"),
      div(tableOutput('total'), style = "font-size:200%"),
      h3("Emails collected:"),
      div(textOutput('email'), style = "font-size:200%")
      
    ),
    
    column(9,  
           
      plotlyOutput('date', height = 300) 
           
    )
            
   ),
   
  br(),
                  
  fluidRow(

    plotlyOutput('country', height= "400px")

  ),
  
  br(),
  
  fluidRow(
           
    column(3,
      plotlyOutput('age')
    ),
  
    column(6,
      tableOutput('empstat'),
      column(5,tableOutput('gender')),
      column(7,tableOutput('education'))
    ),
    
    column(3,
           plotlyOutput('time', height= "400px")
    )
                    
  )
  
)

############# Server #############
server <- function(input, output, session) {

  source("import.r")
  
  levels(ds$B001)[c(28:56,58:60)] <- "Other country"
  ds$date <- format(as.Date(ds$STARTED),format='%d-%m')
  
  output$country <- renderPlotly({
    
    data <- data.frame(table(ds$B001,ds$FINISHED))
    
    fig <- plot_ly(data[data$Var2==TRUE,], x = ~Var1, y = ~Freq, type = 'bar', name = 'Full')
    fig <- fig %>% add_trace(data=data[data$Var2==FALSE,], y = ~Freq, name = 'Partial')
    fig <- fig %>% layout(yaxis=list(title="Responses", hoverformat='.0f'),
                          xaxis=list(title=NA), barmode="stack",
                          title=list(text="Response by country", x = 0))

  })
  
  output$date <- renderPlotly({
    
    data <- data.frame(table(ds$date,ds$FINISHED))
    
    fig <- plot_ly(data[data$Var2==TRUE,], x = ~Var1, y = ~Freq, type = 'bar', name = 'Full')
    fig <- fig %>% add_trace(data=data[data$Var2==FALSE,], y = ~Freq, name = 'Partial')
    fig <- fig %>% layout(yaxis=list(title="Responses", hoverformat='.0f'),
                          xaxis=list(title=NA), barmode="stack",
                          title=list(text="Response by date", x = 0))
    
  })
  
  output$total <- renderTable({
    
    data <- ds %>% 
      select(CASE, FINISHED)
    data$Finished[ds$FINISHED==TRUE] <- "Full"
    data$Finished[ds$FINISHED==FALSE] <- "Partial"
    
    data %>%
      group_by(Finished) %>%
      summarise(Responses = n()) 

  }, colnames = FALSE)
  
  output$time <- renderPlotly({
    
    data <- ds %>%
      select(TIME_SUM, FINISHED) %>%
      mutate(Time = TIME_SUM / 60)
    
    fig <- plot_ly(data=data[data$FINISHED==TRUE,], y = ~Time, type = "box", name="Full")
    fig <- fig %>% add_trace(data=data[data$FINISHED==FALSE,], y = ~Time, name="Partial")
    fig <- fig %>% layout(yaxis=list(title="Minutes"),
                          title=list(text="Time spent", x = 0),
                          showlegend = FALSE)
    
  })
  
  output$age <- renderPlotly({
    
    data <- ds %>%
      select(B003_01, FINISHED) %>%
      filter(!is.na(B003_01))
    
    fig <- plot_ly(data=data[data$FINISHED==TRUE,], y = ~B003_01, type = "box", name="Full")
    fig <- fig %>% add_trace(data=data[data$FINISHED==FALSE,], y = ~B003_01, name="Partial")
    fig <- fig %>% layout(yaxis=list(title="Age"),
                          title=list(text="Respondent age", x = 0),
                          showlegend = FALSE)
    
  })
  
  output$email <- renderText(table(!is.na(ds$F021))[2])
  
  output$empstat <- renderTable({
    
    data <- ds %>% 
      select(D001, FINISHED) %>%
      filter(!is.na(D001))
    data$Finished[data$FINISHED==TRUE] <- "Full"
    data$Finished[data$FINISHED==FALSE] <- "Partial"
    
    data %>%
      group_by(Finished, D001) %>%
      summarise(Responses = n()) %>%
      ungroup() %>%
      spread(Finished, Responses) %>%
      rename('Employment status' = D001)
    
  })
  
  output$gender <- renderTable({
    
    data <- ds %>% 
      select(B002, FINISHED) %>%
      filter(!is.na(B002))
    data$Finished[data$FINISHED==TRUE] <- "Full"
    data$Finished[data$FINISHED==FALSE] <- "Partial"
    
    data %>%
      group_by(Finished, B002) %>%
      summarise(Responses = n()) %>%
      ungroup() %>%
      spread(Finished, Responses) %>%
      rename(Gender = B002)
    
  })
  
  output$education <- renderTable({
    
    data <- ds %>% 
      select(F004, FINISHED) %>%
      filter(!is.na(F004))
    data$Finished[data$FINISHED==TRUE] <- "Full"
    data$Finished[data$FINISHED==FALSE] <- "Partial"
    
    data %>%
      group_by(Finished, F004) %>%
      summarise(Responses = n()) %>%
      ungroup() %>%
      spread(Finished, Responses) %>%
      rename(Education = F004)
    
  })
  
  output$extraction_date <- renderText(
    paste("Survey response at:",
    format(Sys.time(),tz="Etc/GMT-1"))
  )
  
}

shinyApp(ui,server)

