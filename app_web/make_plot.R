#Function for making the plot
make_plot <- function(data) {

  #Variable class
  class <- data[[2]]
  #The dataframe
  data <- data[[1]] %>%
    droplevels()
  #Droplevels here because we want to get rid of any empty breakdown categories.

  #Setting the axis label depending on the type of variable
  if (class=="numeric") {x_label <- "Mean"} else {x_label <- "%"}
    
  breakdown <- colnames(data)[1]
  colnames(data)[1] <- "bdown"
  
  #Making the plot
  fig <- plot_ly()
  
  #If numeric then just add one trace
  if (class=="numeric") {

    fig <- fig %>% add_trace(y=data[[1]], x = ~Mean, name = 'Mean', data=data, type='bar')
    
    # To order the plot, we need to specify its ordering in an array
    order <- data[order(-data$Mean),]
    
  } else {
    
    # if categorical, loop over the categories adding a trace each time
    cnames <- colnames(data)[2:length(colnames(data))]
    
    for (cname in cnames) {
      
      fig <- fig %>% add_trace(y=data[[1]], x = data[[cname]], 
                               name = cname, data=data, type='bar')    
      
    }
  
    # To order the plot, we need to specify its ordering in an array
    order <- data %>%
      mutate(sum = rowSums(.[cnames]))
    order <- order[order(-order$sum),]  
    
  }
  
  if (breakdown!="Country") {
    
    order <- data[[1]]
    
  }
  
  #Add layout elements
  fig <- fig %>% layout(xaxis = list(title = x_label,
                                     hoverformat='.0f'), 
                        yaxis=list(title=NA, autorange="reversed",
                                   categoryorder = "array",
                                   categoryarray = order[[1]]),
                        barmode = 'stack',
                        hovermode = 'compare',
                        colorway=EF_colours,
                        margin = list(r=100),
                        images = list(list(source = "https://upload.wikimedia.org/wikipedia/en/4/45/Eurofound_Logo_2016.png",
                             xref = "paper",
                             yref = "paper",
                             x= 1,
                             y= 0.15,
                             sizex = 0.15,
                             sizey = 0.15,
                             opacity = 0.6))
                        )
  
  #Removing buttons from the modebar
  fig <- fig %>% config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d","pan2d",
                                                   "select2d","lasso2d","autoScale2d",
                                                   "toggleSpikelines","hoverClosestCartesian",
                                                   "hoverCompareCartesian"),
                        displaylogo = FALSE)
}

