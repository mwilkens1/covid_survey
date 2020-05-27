#Function for making the plot
make_plot <- function(data, mobile) {

  #Variable class
  class <- data[[1]][[2]]
  #Range of the axis
  range <- data[[1]][[4]]
  
  #First check how many elements the list 'data' has to determine the number of time periods
  periods <- length(data)
  
  # In case it is only only period
  if (periods==1) {
    
    period <- data[[1]][[7]]
    
    #The dataframe will be the first elemement of the first and only list of the list
    data <- data[[1]][[1]] %>%
      droplevels()
    #Droplevels here because we want to get rid of any empty breakdown categories.
    #Creating a variable containing the period
    data$period <- period
    
  } else {
    
    # if more than 1 period we need to combine the datasets of different periods
    # into one period. The first list of 'data' will be the first period (April/May)
    # and the second will be the second period (June / July)
    
    data_1 <- data[[1]][[1]]
    data_1$period <- data[[1]][[7]]
    data_2 <- data[[2]][[1]]
    data_2$period <- data[[2]][[7]]
    
    # The *'s have been added to the axis labels (e.g. countries) in make_data. However,
    # some labels may be unreliable in one wave but not in the other. This code ensures that 
    # there is an * in case there is unreliable data in one or both of the waves. The footnote 
    # explains which one
    
    categories_wave1 <- levels(data_1[[1]])
    categories_wave2 <- levels(data_2[[1]])

    #For each level of the breakdown variable in the first wave
    for (l in seq(1,length(categories_wave1),1)) {

      #Check if wave1 category contains a *
      #If so use the level for that wave
      if (grepl("*", categories_wave1[l], fixed = TRUE)) {
       
        levels(data_1[[1]])[l] <- categories_wave1[l]
       
      }
           
      if (grepl("*", categories_wave2[l], fixed = TRUE)) {
        
        levels(data_1[[1]])[l] <- categories_wave2[l]
        
      }
      
      if (grepl("*", categories_wave1[l], fixed = TRUE)) {
        
        levels(data_2[[1]])[l] <- categories_wave1[l]
        
      }
      
      if (grepl("*", categories_wave2[l], fixed = TRUE)) {
        
        levels(data_2[[1]])[l] <- categories_wave2[l]
        
      }
      
    }
    
    data <- rbind(data_1, data_2) %>%
      droplevels()

  }
  
  #Setting the axis label depending on the type of variable
  if (class=="numeric") {x_label <- "Mean"} else {x_label <- "%"}
    
  breakdown <- colnames(data)[1]
  colnames(data)[1] <- "bdown"
  
  #setting the height for the plot, which depends on the breakdown
  if      (breakdown=="Country")           {height <- 500} 
  else if (breakdown=="Employment status") {height <- 400}
  else                                     {height <- 300}
  
  # Adding a space to the categories to increase the distance to the axis
  newlevels <- NULL
  for (level in levels(data$bdown)) {
    
    newlevels <- c(newlevels,paste0(level," "))
    
  }
  
  data$bdown <- factor(data$bdown, levels = levels(data$bdown), labels=newlevels)
  
  #Making the plot
  fig <- plot_ly()
  
  #If numeric then just add one trace
  if (class=="numeric") {
    
    if (periods==1) {
      
      fig <- fig %>% add_trace(y=data[[1]], x = ~Mean, 
                               data=data, type='bar') 
      
      
    } else {
      
      fig <- fig %>% add_trace(y=data[[1]], 
                        x = ~Mean, name = data$period, 
                        data=data, 
                        type='scatter', mode='markers',
                        marker=list(size=12), opacity=0.8)  
      
    }
    
    # To order the plot, we need to specify its ordering in an array
    order <- data[order(-data$Mean),][[1]]
    
  } else {# if categorical,

      # If only 1 time period selected
      
      if (periods==1) {
      
      # loop over the categories adding a trace each time
        cnames <- colnames(data)[2:(length(colnames(data))-2)]
    
        for (cname in cnames) {
          
          fig <- fig %>% add_trace(y=data[[1]], x = data[[cname]], 
                                   name = cname, data=data, type='bar')    
          
        }
        
      } else {#if more than 1 time period: only show the total
    
    
          fig <- fig %>% add_trace(y=data[[1]], x = data[['Total']], 
                                 name = data$period, data=data, 
                                 type='scatter', mode='markers',
                                 marker=list(size=12), opacity=0.8)  
    
      }
  
  
    # To order the plot, we need to specify its ordering in an array
    order <- data[order(-data$Total),][[1]]
    
  }
  
  if (breakdown!="Country") {
    
    order <- levels(data[[1]])
    
  }
  
  font_size <- ifelse(mobile==TRUE,9,12)
  margin    <- ifelse(mobile==TRUE,0,100)
  
  #Add layout elements
  fig <- fig %>% layout(xaxis = list(title = x_label,
                                     hoverformat='.1f'), 
                        yaxis=list(title=NA, autorange="reversed",
                                   categoryorder = "array",
                                   categoryarray = order,
                                   dtick = 1),
                        font = list(size=font_size),
                        barmode = 'stack',
                        hovermode = 'compare',
                        colorway=EF_colours,
                        margin = list(r=margin))
  
  #Change the hovermode for multiple periods
  if (periods>1) {
    
    fig <- fig %>% layout(hovermode = 'y')
    
  }
  
  if (mobile==FALSE) {
    
    fig <- fig %>% layout(images = list(list(
      source = "https://upload.wikimedia.org/wikipedia/en/4/45/Eurofound_Logo_2016.png",
      xref = "paper",
      yref = "paper",
      x= 1,
      y= 0.15,
      sizex = 0.15,
      sizey = 0.15,
      opacity = 0.6)))
    
  } else {
    
    fig <- fig %>% layout(legend = 
                            list(xanchor = "left",
                                 yanchor = "top",
                                 x = 0.5,
                                 orientation="h"),
                          xaxis=list(fixedrange=TRUE),
                          yaxis=list(fixedrange=TRUE)) %>%
                    config(displayModeBar = F)
    }

  
  #Adding custom range for the x-axis if a range exists
  if (!is.null(range) & class=="numeric") {

    fig <- fig %>% layout(xaxis = 
              list(range = c(range[1],
                             ceiling(max(data$Mean)))
                   ))
      
  }
  
  #Remove legend if variable is numeric and there is only one period
  if (class=="numeric" & periods==1) {
    
    fig <- fig %>% layout(showlegend = FALSE)
    
  }
  
  #Removing buttons from the modebar
  fig <- fig %>% config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d","pan2d",
                                                   "select2d","lasso2d","autoScale2d",
                                                   "toggleSpikelines","hoverClosestCartesian",
                                                   "hoverCompareCartesian"),
                        toImageButtonOptions = list(
                          format = "png",
                          width = 894, #width of the plot in the iframe
                          height = height), #depends on the breakdown
                        displaylogo = FALSE)
}


