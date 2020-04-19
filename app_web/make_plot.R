#Function for making the plot
make_plot <- function(inputvar, inputcat, data) {
  
  # Show a message if for a categorical variable no categories are selected
  validate(
    need(
      (!is.null(inputcat) | class(ds[[inputvar]])=="numeric"), 
      "Select at least one category")
  )
  
  #Variable class
  class <- data[[2]]
  #The dataframe
  data <- data[[1]]
  
  colnames(data)[1] <- "bdown"
  
  #Setting the axis label depending on the type of variable
  if (class=="numeric") {
    x_label <- "Mean"    
  } else {
    x_label <- "%"
  }
  
  #Making the plot
  fig <- plot_ly(orientation='h')
  
  #If numeric then just add one trace
  if (class=="numeric") {
    
    fig <- fig %>% add_trace(y=data[[1]], x = ~Mean, name = 'Mean', data=data, type='bar')
    
  } else {
    
    # if categorical, loop over the categories adding a trace each time
    cnames <- colnames(data)[2:length(colnames(data))]
    
    for (cname in cnames) {
      
      fig <- fig %>% add_trace(y=data[[1]], x = data[[cname]], name = cname, data=data, type='bar')    
      
    }
    
  }
  
  #Add layout elements
  fig <- fig %>% layout(xaxis = list(title = y_label,
                                     hoverformat='.1f'), 
                        yaxis=list(title=NA),
                        barmode = 'stack',
                        hovermode = 'compare',
                        colorway=EF_colours)
  
  #Removing buttons from the modebar
  fig <- fig %>% config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d","pan2d",
                                                   "select2d","lasso2d","autoScale2d",
                                                   "toggleSpikelines","hoverClosestCartesian",
                                                   "hoverCompareCartesian"),
                        displaylogo = FALSE)
  

}