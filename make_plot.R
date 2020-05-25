#Function for making the plot
make_plot <- function(data, mobile) {

  #Variable class
  class <- data[[2]]
  #Range of the axis
  range <- data[[4]]
  #The dataframe
  data <- data[[1]] %>%
    droplevels()
  #Droplevels here because we want to get rid of any empty breakdown categories.

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

    fig <- fig %>% add_trace(y=data[[1]], x = ~Mean, name = 'Mean', data=data, type='bar')
    
    # To order the plot, we need to specify its ordering in an array
    order <- data[order(-data$Mean),][[1]]
    
  } else {

    # if categorical, loop over the categories adding a trace each time
    cnames <- colnames(data)[2:(length(colnames(data))-1)]

    for (cname in cnames) {
      
      fig <- fig %>% add_trace(y=data[[1]], x = data[[cname]], 
                               name = cname, data=data, type='bar')    
      
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
                                   categoryarray = order),
                        font = list(size=font_size),
                        barmode = 'stack',
                        hovermode = 'compare',
                        colorway=EF_colours,
                        margin = list(r=margin))
  
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

