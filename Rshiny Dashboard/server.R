#################### server.R ####################
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

draw <- vc

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = -73.97, lat = 40.75, zoom = 13)
  })
  

  
  # Choose just one vehicle
  drawvalue <- reactive({if (input$vehicle == ''){return(vc)}else{
    t <- filter(vc, vehicle_type_code1 == input$vehicle | vehicle_type_code2 == input$vehicle | daytime == input$daytime)
    return(t)
  }})
  
  dt <- reactive({
    subset(drawvalue(), daytime %in% input$daytime)
  })
  
  output$daytimeData <- renderTable({ dt() })
 
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    colorBy <- "vehicle_type_code1"
    sizeBy <- input$size
    draw <- drawvalue()
    
    colorData <- draw[[colorBy]]
    if (colorBy == "number_of_persons_injured"|colorBy == "number_of_persons_killed") {
      pal <- colorBin(heat.colors(7), colorData, 7)} else{
        pal <- colorFactor("Set2", colorData)
      }
    
    radius <- draw[[sizeBy]] / 9 * 250 + 50
    
    if (input$cluster == TRUE){
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        showGroup('Cluster') %>%
        addCircles(~longitude, ~latitude, radius=radius, group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addCircleMarkers(~longitude, ~latitude, radius = 0, group = "Cluster",
                         clusterOptions = markerClusterOptions())%>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }else{
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        hideGroup('Cluster') %>%
        addCircles(~longitude, ~latitude, radius=radius, group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) #%>%
        #addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  #layerId="colorLegend")
    }
  })

  # Show a popup at the given location
  showvcPopup <- function(eventid, lat, lng) {
    draw <- drawvalue()
    selectedvc <- filter(draw, latitude == lat, longitude == lng)
    entry <- function(row){
      result <- as.character(tagList(
        tags$h6(HTML(sprintf("Reasons: %s & %s",row[2], row[3]))),
        tags$strong(HTML(sprintf("%s & %s", row[9], row[10]))), tags$br(),
        sprintf("Vehicles: %s & %s", row[23], row[24]), tags$br(),
        sprintf("Factors: %s & %s", row[20], row[21]), tags$br(),
        sprintf("%s Injuries & %s Deaths", row[18], row[19]), tags$br()))
      return(result)
    }
    content <- apply(selectedvc, 1, entry)
    content <- paste0(content, collapse = "\n")
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = eventid)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showvcPopup(event$id, event$lat, event$lng)
    })
  })
  
  ## See Your Neighbourhood ###########################################
  
  observe({
    zipcodes <- if (is.null(input$boroughs)) character(0) else {
      filter(cleantable, Borough %in% input$boroughs) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  # When actions is clicked, call popup function for the corresponding latitude and longitude
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.02
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showvcPopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$vctable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        is.null(input$boroughs) | Borough %in% input$boroughs,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Borough, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  ## Most Dangerous Intersections ###########################

  output$toptable <- DT::renderDataTable({
    df <- most_dangerous%>%#read.csv('data/Most_Dangerous_Intersections.csv') %>%
      #mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Borough, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
})
