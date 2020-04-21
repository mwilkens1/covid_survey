# Function for the description under the plot
make_description <- function(category, inputvar) {
  
  class <- class(ds[[inputvar]])[length(class(ds[[inputvar]]))]
  
  if (class=="factor") {
    
    text <- "The figure shows the the percentage that answered:"
    
    #Loop over categories if more than 1 selected
    if (length(category)>1) {
      
      for (c in category) {
        
        #Only for those with more than 2 categories selected
        # If not the final two categories
        if ((length(category)>2) & (c %in% category[1:(length(category)-2)])) {

          text <- paste0(text," '",c,"',")
          
        }    
        
        # If the category before the last, use 'or'
        if (c==category[length(category)-1]) {

          text <- paste0(text," '",c,"' or")
          
          # If it is the final category, dont add comma
        } else if (c==category[length(category)]) {

          text <- paste0(text," '",c,"'.")
          
        }
        
      }
      
      # If only one category seleted
    } else {
      
      text <- paste0(text," '",category,"'.")
      
    }
    
    return(text)
    
    # If numeric variable
  } else {
    
    return("The figure shows the mean")
    
  }
  
}