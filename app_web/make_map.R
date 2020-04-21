make_map <- function(inputvar, inputcat, data) {
  
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
  
  # Show a message if no countries are selected
  validate(
    need(
      (sum(levels(data$Country) %in% levels(shp_20$NAME_ENGL)) > 0 ), 
      "Select at least one country")
  )
  
  #Setting the axis label depending on the type of variable
  if (class=="numeric") {x_label <- "Mean"} else {x_label <- "%"}
  
  #Only keeping countries in the shapefile that are also in the data
  shp_20 <- shp_20 %>% 
    subset(Country %in% levels(data$Country)) 
  
  #This creates the color palette based on the EF styleguide
  EF_aqua <- colorRampPalette(c("#AACBE9","#036D9C"))
  
  draw_map <- function(var) {
    
    #Changing to character to avoid a warning
    shp_20@data$Country <- as.character(shp_20@data$Country)
    data$Country <- as.character(data$Country)
    
    #Joining the survey data to the map data by country name
    shp_20@data <- shp_20@data %>%
      left_join(data, by="Country") 
    
    #Number of bins for the plot
    bins <- 6
    
    #This creates the palette for the plot
    pal <- colorBin(EF_aqua(bins), domain=shp_20[[var]], bins=bins)
    
    #This creats the popup labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%g",
      shp_20$Country, round(shp_20[[var]],1)) %>% 
      lapply(HTML)
    
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
      #Sets the default view and zoom level
      setView(14,52.2, 4) %>%
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
                opacity = 0.8) %>%
      #Adding the logo. This is from package leafem
      addLogo("https://upload.wikimedia.org/wikipedia/en/4/45/Eurofound_Logo_2016.png",
                alpha = 0.8, src = "remote",
                position = "topright",
                offset.x = 0, offset.y = 0, width = 86, height = 60)
  }
  
  #Calling the function
  if (class == "numeric") {
    
    #Variable 'mean' if numeric
    fig <- draw_map("Mean")
    
  } else {
    
    #If categorical... 
    cnames <- colnames(data)[2:length(colnames(data))]
    
    #... sum the percentages of the selected categories
    data <- data %>% 
      mutate(sum = rowSums(.[cnames])) 
    
    #And pass on 'sum' as the variable to the function
    fig <- draw_map("sum")
    
  }
  
}