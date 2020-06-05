library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyr)
library(dplyr)
library(rlist)

#setwd(paste0(getwd(),"/dashboard"))

############## User interface #############
ui <- fluidPage( style = 'width:1200px;',

   h1("Eurofound survey: Living, Working and COVID-19"),
   h4(textOutput("extraction_date")),
   br(),
   
   fluidRow(
     column(3,
       materialSwitch(
         inputId = "clean",
         label = "Clean responses only",
         value = TRUE,
         status = "primary"
       )),
   
    column(2,
     materialSwitch(
       inputId = "email",
       label = "Email only",
       value = FALSE,
       status = "primary"
     )),
    
    column(4,
       dateRangeInput("date_filter", label = "Date range",
                      start = "2020-04-09")
       ),
    
    column(3,
       pickerInput("gender_filter", label = "Gender",
                   choices = c("All","Male","Female","In another way"),
                   selected = "All")
      )
    
    ),
   

   
   fluidRow(
     
     column(3,
        pickerInput("country_filter", label = "Country",
                    choices = c("All","Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany","Greece",        
                                "Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia",      
                                "Slovenia","Spain","Sweden","United Kingdom","Other country"),
                    selected = "All"
        )
     ),
     column(3,
            
        sliderInput("age_filter", label = "Age", min = 0, 
                    max = 100, value = c(0, 100))

     ),
     
     column(3,
            
        pickerInput("empstat_filter", label = "Employment status",
                    choices = c("All","Employee","Self-employed with employees","Self-employed without employees","Unemployed",
                                "Unable to work due to long-term illness or disability","Retired",
                                "Full-time homemaker/fulfilling domestic tasks","Student"),
                    selected = "All"
        )
          
    ),
    
    column(3,
           
         pickerInput("education_filter", label = "Education",
                     choices = c("All","Primary","Secondary","Tertiary"),
                     selected = "All"
         )
           
    )
     
   ),

   br(),   
                 
   fluidRow(
            
    column(3,
      
      h3("Response:"),
      div(tableOutput('total'), style = "font-size:200%"),
      h3("Emails collected:"),
      div(textOutput('email_count'), style = "font-size:200%")
      
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
                    
  ),
  
  br(),
  
  fluidRow(
    
    plotlyOutput('country_gender', height="400px"),
    plotlyOutput('country_age', height="400px"),
    plotlyOutput('country_education', height="400px")
    
  )
  
)

############# Server #############
server <- function(input, output, session) {
 
  progressSweetAlert(
    session = session, id = "load_data",
    title = "Fetching data",
    display_pct = TRUE, value = 20
  )
   
  source("import.r", local=TRUE)
  
  ds$date <- as.Date(as.POSIXct(ds$STARTED),format='%d-%m')
  
  data <- reactive({
     
     ds <- ds %>%
       #filter for clean responses
       {if (input$clean==TRUE) filter(.,clean==TRUE) else .} %>%
       #filter for emails only
       {if (input$email==TRUE) filter(.,!is.na(F021)) else .} %>%
       #filter date range
       filter(date>=input$date_filter[1],
              date<=input$date_filter[2]) %>%
       #Country filter
       {if (input$country_filter!="All") filter(.,B001==input$country_filter) else .} %>%
       #Gender filter
       {if (input$gender_filter!="All")  filter(.,B002==input$gender_filter) else .} %>%
       #age filter
       filter(B003_01>=input$age_filter[1],
              B003_01<=input$age_filter[2]) %>%
       #employment status filter
       {if (input$empstat_filter!="All")  filter(.,D001==input$empstat_filter) else .} %>%
       {if (input$education_filter!="All")  filter(.,F004==input$education_filter) else .}
     
  })
  

  output$country <- renderPlotly({
    
    ds <- data()
    
    data <- data.frame(table(ds$B001,ds$FINISHED))
    
    fig <- plot_ly(data[data$Var2==TRUE,], x = ~Var1, y = ~Freq, type = 'bar', name = 'Full')
    fig <- fig %>% add_trace(data=data[data$Var2==FALSE,], y = ~Freq, name = 'Partial')
    fig <- fig %>% layout(yaxis=list(title="Responses", hoverformat='.0f'),
                          xaxis=list(title=NA), barmode="stack",
                          title=list(text="Response by country", x = 0),
                          hovermode = 'compare')

  })
  
  output$date <- renderPlotly({
    
    ds <- data()
    
    data <- data.frame(table(ds$date,ds$FINISHED))
    
    fig <- plot_ly(data[data$Var2==TRUE,], x = ~Var1, y = ~Freq, type = 'bar', name = 'Full')
    fig <- fig %>% add_trace(data=data[data$Var2==FALSE,], y = ~Freq, name = 'Partial')
    fig <- fig %>% layout(yaxis=list(title="Responses", hoverformat='.0f'),
                          xaxis=list(title=NA, type="date"), barmode="stack",
                          title=list(text="Response by date", x = 0),
                          hovermode = 'compare')
    
  })
  
  output$total <- renderTable({
    
    ds <- data()
    
    data <- ds %>% 
      select(FINISHED)
    data$Finished[ds$FINISHED==TRUE] <- "Full"
    data$Finished[ds$FINISHED==FALSE] <- "Partial"
    
    data %>%
      group_by(Finished) %>%
      summarise(Responses = n()) 

  }, colnames = FALSE)
  
  output$time <- renderPlotly({
    
    ds <- data()
    
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
    
    ds <- data()
    
    data <- ds %>%
      select(B003_01, FINISHED) %>%
      filter(!is.na(B003_01))
    
    fig <- plot_ly(data=data[data$FINISHED==TRUE,], y = ~B003_01, type = "box", name="Full")
    fig <- fig %>% add_trace(data=data[data$FINISHED==FALSE,], y = ~B003_01, name="Partial")
    fig <- fig %>% layout(yaxis=list(title="Age"),
                          title=list(text="Respondent age", x = 0),
                          showlegend = FALSE)
    
  })
  
  output$country_gender <- renderPlotly({
    
    ds <- data()
    
    data <- ds %>%
      select(B001, B002, FINISHED) %>%
      filter(!is.na(B002) & FINISHED==TRUE)
    
    data <- data.frame(table(data$B001,data$B002))

    fig <- plot_ly(data[data$Var2=="Male",], x = ~Var1, y = ~Freq, type = 'bar', name = 'Men')
    fig <- fig %>% add_trace(data=data[data$Var2=="Female",], y = ~Freq, name = 'Women')
    fig <- fig %>% layout(yaxis=list(title="Responses", hoverformat='.0f'),
                          xaxis=list(title=NA), barmode="stack",
                          title=list(text="Response by country and gender", x = 0),
                          hovermode = 'compare')
  })
  
  output$country_age <- renderPlotly({
    
    ds <- data()
    
    data <- ds %>%
      select(B001, age_group, FINISHED) %>%
      filter(!is.na(age_group) & FINISHED==TRUE) 
    
    data <- data.frame(table(data$B001,data$age_group))
    
    fig <- plot_ly(data[data$Var2=="Under 35",], x = ~Var1, y = ~Freq, type = 'bar', name = 'Under 35')
    fig <- fig %>% add_trace(data=data[data$Var2=="35 - 49",], y = ~Freq, name = '35 - 49')
    fig <- fig %>% add_trace(data=data[data$Var2=="50 and over",], y = ~Freq, name = '50 and over')
    fig <- fig %>% layout(yaxis=list(title="Responses", hoverformat='.0f'),
                          xaxis=list(title=NA), barmode="stack",
                          title=list(text="Response by country and age group", x = 0),
                          hovermode = 'compare')
  })
  
  output$country_education <- renderPlotly({
    
    ds <- data()
    
    data <- ds %>%
      select(B001, F004, FINISHED) %>%
      filter(!is.na(F004) & FINISHED==TRUE) 
    
    data <- data.frame(table(data$B001,data$F004))
    
    fig <- plot_ly(data[data$Var2=="Primary",], x = ~Var1, y = ~Freq, type = 'bar', name = "Primary")
    fig <- fig %>% add_trace(data=data[data$Var2=="Secondary",], y = ~Freq, name = "Secondary")
    fig <- fig %>% add_trace(data=data[data$Var2=="Tertiary",], y = ~Freq, name = "Tertiary")
    fig <- fig %>% layout(yaxis=list(title="Responses", hoverformat='.0f'),
                          xaxis=list(title=NA), barmode="stack",
                          title=list(text="Response by country and education", x = 0),
                          hovermode = 'compare')
  })
  
  output$email_count <- renderText({
    
    ds <- data()
    
    table(!is.na(ds$F021))[2]
    
    })
  
  updateProgressBar(
    session = session,
    id = "load_data",
    value = 95
  )
  
  output$empstat <- renderTable({
    
    ds <- data()
    
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
    
    ds <- data()
    
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
    
    ds <- data()
    
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
 
  closeSweetAlert(session = session) 
  
}

shinyApp(ui,server)

