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
  
  #Setting the axis label depending on the type of variable
  if (class=="numeric") {x_label <- "Mean"} else {x_label <- "%"}
  
  shp_20 <- shp_20 %>% 
    subset(Country %in% levels(data$Country)) 
  
  EF_aqua <- colorRampPalette(c("#AACBE9","#036D9C"))
  
  draw_map <- function(var) {
    
    shp_20@data <- shp_20@data %>%
      left_join(data, by="Country") 
    
    bins <- 6
    pal <- colorBin(EF_aqua(bins), domain=shp_20[[var]], bins=bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g",
      shp_20$Country, round(shp_20[[var]],1)) %>% 
      lapply(htmltools::HTML)
    
    if (class=="numeric") { 
      title <- "Mean" 
      suffix <- NULL
    } else { 
      title <- NULL
      suffix <- "%"
    }
    
    fig <- leaflet(shp_20) %>% 
      setView(14,52.2, 4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
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
      addLegend("bottomright", pal = pal, 
                values = as.formula(paste("~",var)),
                title = title,
                labFormat = labelFormat(suffix = suffix),
                opacity = 0.8) %>%
      addLogo("https://upload.wikimedia.org/wikipedia/en/4/45/Eurofound_Logo_2016.png",
                alpha = 0.8, src = "remote",
                position = "topright",
                offset.x = 0, offset.y = 0, width = 86, height = 60)

    }
  
  if (class == "numeric") {
    
    fig <- draw_map("Mean")
    
  } else {
    
    cnames <- colnames(data)[2:length(colnames(data))]
    
    data <- data %>% 
      mutate(sum = rowSums(.[cnames])) 
    
    fig <- draw_map("sum")
    
  }
  
}