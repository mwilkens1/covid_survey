make_map <- function(data) {

  #Variable class
  class <- data[[2]]
  #The dataframe
  data <- data[[1]]
  
  #Setting the axis label depending on the type of variable
  if (class=="numeric") {x_label <- "Mean"} else {x_label <- "%"}
  
  #Only keeping countries in the shapefile that are also in the data
  shp_20 <- shp_20 %>% 
    subset(Country %in% levels(data$Country)) 

  #This creates the color palette based on the EF styleguide
  #EF_aqua <- colorRampPalette(c("#AACBE9","#036D9C"))
  #EF colours are not working, using this color brewer gradient
  colors <- c('#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')
   
  draw_map <- function(var) {
    
    #Changing to character to avoid a warning
    shp_20@data$Country <- as.character(shp_20@data$Country)
    data$Country <- as.character(data$Country)
    
    #Joining the survey data to the map data by country name
    shp_20@data <- shp_20@data %>%
      left_join(data, by="Country") 
    
    #Number of bins for the plot
    bins <- 8
   
    #This creates the palette for the plot
    pal <- colorBin(colors, domain=shp_20[[var]], bins=bins)
    
    #This creats the popup labels
    #It shows 'insufficient data' in case the country has been excluded because of 
    #a lack of data and the value otherwise.
    labels <- {
      
      sprintf("<strong>%s</strong><br/>%s",
                shp_20$Country, as.character(
                  
                  ifelse(is.na(shp_20[[var]]),
                  "Insufficient data",
                  round(shp_20[[var]],0))
              
              )
      )
      
    } %>% lapply(HTML)
    
    #Setting the title and suffix of the legend depending on class
    if (class=="numeric") { 
      
      title <- "Mean" 
      suffix <- NULL
      
    } else { 
      
      title <- NULL
      suffix <- "%"
      
    }
    
    # Plotting the map
    # Initialising leaflet with the shapefile
    fig <- leaflet(shp_20, 
                   #This removes the zoomcontrol 
                   options = leafletOptions(zoomControl = FALSE)) %>%
                    #this puts it back again in another position to avoid overlap
                    onRender("function(el, x) {
                    L.control.zoom({ position: 'bottomleft' }).addTo(this)}") %>% 
      #Adding the logo. This is from package leafem
      addLogo("https://upload.wikimedia.org/wikipedia/en/4/45/Eurofound_Logo_2016.png",
              alpha = 0.8, src = "remote",
              position = "topright",
              offset.x = 0, offset.y = 0, width = 86, height = 60) %>%
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
                labFormat = labelFormat(suffix = suffix),
                opacity = 0.8,
                na.label = "Insufficient data") %>%
      #Sets the default view and zoom level
      setView(14,52.2, 4)
     
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