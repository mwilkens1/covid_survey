make_map <- function(data, mobile) {

  # Variable class
  # Data is a list of lists (see make data)
  # this takes the second element of the first list: the class
  # the map is only showing if there is only 1 time period selected so the
  # list will always only have 1 list in it for maps. (not for plots, see make_plot)
  class <- data[[1]][[2]]
  # this takes the first element of the first list: the dataset
  # The dataframe
  data <- data[[1]][[1]]
  
  #Setting the axis label depending on the type of variable
  if (class=="numeric") {x_label <- "Mean"} else {x_label <- "%"}
  
  #Only keeping countries in the shapefile that are also in the data
  shp_20 <- shp_20 %>% 
    #Not taking into account any flags
    subset(Country %in% gsub("[*]","",levels(data$Country))) 

  #This creates the color palette based on the EF styleguide
  #EF_aqua <- colorRampPalette(c("#AACBE9","#036D9C"))
  #EF colours are not working, using this color brewer gradient
  colors <- c('#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')
   
  draw_map <- function(var) {
    
    #Changing to character to avoid a warning
    shp_20@data$Country <- as.character(shp_20@data$Country)
    data$Country_flagged <- as.character(data$Country)
    data$Country <- gsub("[*]","",as.character(data$Country))
    
    #Joining the survey data to the map data by country name
    shp_20@data <- shp_20@data %>%
      left_join(data, by="Country") 
    
    shp_20@data$Country_flagged[is.na(shp_20@data$Country_flagged)] <- 
      shp_20@data$Country[is.na(shp_20@data$Country_flagged)]
    
    #Number of bins for the plot
    bins <- 8
   
    #This creates the palette for the plot
    pal <- colorBin(colors, domain=shp_20[[var]], bins=bins)
    
    #This creats the popup labels
    #It shows 'insufficient data' in case the country has been excluded because of 
    #a lack of data and the value otherwise.
    
    #Setting the title and suffix of the legend depending on class
    if (class=="numeric") { 
      
      title <- "Mean" 
      suffix <- NULL
      
    } else { 
      
      title <- NULL
      suffix <- "%"
      
    }
    
    labels <- {
      
      sprintf("<strong>%s</strong><br/>%s",
              
              #First placeholder
              shp_20$Country_flagged,
               
              #second placeholder
              as.character(
                  
                  ifelse(is.na(shp_20[[var]]),
                  "Insufficient data",
                  paste0(round(shp_20[[var]],1),suffix))
              
              )
      )
      
    } %>% lapply(HTML)
    
    zoom <- ifelse(mobile,3,4)
    logoscale <- ifelse(mobile,0.8,1)
    
    # Plotting the map
    # Initialising leaflet with the shapefile
    fig <- leaflet(shp_20) %>%
      #Using CartoDB positron as the map in the background
      addProviderTiles(providers$CartoDB.Positron) %>%
      #Adding the polygons
      addPolygons(fillColor = as.formula(paste("~pal(",var,")")), 
                  fillOpacity = 0.8, 
                  opacity=0.1, 
                  color = "white",
                  weight = 5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "black",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 padding = "3px 8px"),
                    textsize = "12px",
                    direction = "auto")) %>%
      #Adding the legend
      addLegend("bottomright", pal = pal, 
                values = as.formula(paste("~",var)),
                title = title,
                labFormat = labelFormat(suffix = suffix,
                                        digits = 1),
                opacity = 0.8,
                na.label = "Insufficient data") %>%
      #Sets the default view and zoom level
      setView(12,52.2, zoom) %>% 
      #Adding the logo. This is from package leafem
      addLogo("https://www.eurofound.europa.eu/sites/default/files/efcovid19logo.png",
              alpha = 0.8, src = "remote",
              position = "topright",
              offset.x = 0, offset.y = 0, width = logoscale*86, height = logoscale*60)
     
  }
  
  #Calling the function
  if (class == "numeric") {
    
    #Variable 'mean' if numeric
    fig <- draw_map("Mean")
    
  } else {
    
    #If categorical... 
    fig <- draw_map("Total")
    
  }
  
}