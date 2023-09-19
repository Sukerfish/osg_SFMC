server <- function(input, output, session) {
  
  serialRead <- reactive({
    invalidateLater(2500, session)
    out <- read.serialConnection(conn)
    
    out
  })
  
  output$serial <- renderText({gps()})
  
  gps <- reactive({
    gps <- parse.IIGGA(serialRead(), Sys.time()) %>%
      select(latitude, longitude)
    
    gps
  })
  
  vtg <- reactive({
    vtg <- parse.IIVTG(serialRead()) %>%
      select(headingT, speedN)
    
    vtg
  })
  
  hdg <- reactive({
    out <- parse.IIHDT(serialRead()) %>%
      select(heading)
    
    out
  })
  
  #autoInvalidate <- reactiveTimer(3000, NULL)
  
  #get deployed gliders
  deployedGliders <- read.csv("/echos/deployedGliders.txt", 
                              sep = "",
                              header = FALSE)
  colnames(deployedGliders)[1] = "Name"
  colnames(deployedGliders)[2] = "ahrCap"
  
  #only process "real" ones
  deployedGliders <- deployedGliders %>%
    filter(!str_starts(Name,"#")) #remove any commented lines
  
  #initialize list
  mapList <- list()
  gotoList <- list()
  for (i in deployedGliders$Name){
    
    glide <- new.env()
    #load latest live data file
    load(paste0("/echos/", i, "/glider_live.RData"), envir = glide)
    
    mapList[[i]] <- glide$gliderdf
    gotoList[[i]] <- glide$toGliderList
    
  }
  
  updateCheckboxGroupInput(session, "gliders", label = NULL, choices = unique(deployedGliders$Name), c(unique(deployedGliders$Name)))
  
  #fullDF <- data.frame() #initialize
  fullDF <- bind_rows(mapList, .id = "gliderName")
  gotoDF <- bind_rows(gotoList, .id = "gliderName")
  
  output$missionmapLive <- renderLeaflet({
    #establish time threshold 
    gliderDay <- interval(max(fullDF$m_present_time), #lower bound
                          max(fullDF$m_present_time) - hours(input$hours)  #upper bound
    ) 
    
    ###### live mission map #########
    
    #massage gps data a lot
    mapDF <- fullDF %>%
      select(m_present_time, m_gps_lon, m_gps_lat, gliderName) %>%
      filter(!is.na(m_gps_lat)) %>%
      filter(m_present_time %within% gliderDay) %>%
      #filter(gliderName %in% input$gliders) %>%
      mutate(latt = format(m_gps_lat, nsmall = 4),
             longg = format(m_gps_lon, nsmall = 4)) %>% #coerce to character keeping zeroes out to 4 decimals
      mutate(lat = gliderGPS_to_dd(latt),
             long = gliderGPS_to_dd(longg)) %>%
      filter(lat >= -90 & lat <= 90) %>% #remove illegal values
      filter(long >= -180 & long <= 180) %>%
      mutate(gliderName = as.factor(gliderName))
    
    startDF <- mapDF %>%
      group_by(gliderName) %>%
      slice_head()
    
    endDF <- mapDF %>%
      group_by(gliderName) %>%
      slice_tail()
    
    mapSF <- mapDF %>%
      st_as_sf(coords = c("long", "lat"))  %>%
      group_by(gliderName) %>% 
      mutate(seg_end = lead(geometry)) %>%
      rowwise() %>%
      mutate(geometry = st_union(geometry, seg_end) %>% st_cast("LINESTRING")) %>%
      ungroup() %>%
      select(!starts_with("seg"))
    
    pal <- colorFactor("PuOr", mapDF$gliderName)
    
    #get commanded wpt
    cwpt <- fullDF %>%
      select(m_present_time, c_wpt_lat, c_wpt_lon, gliderName) %>%
      filter(m_present_time %within% gliderDay) %>%
      #filter(gliderName %in% input$gliders) %>%
      filter(!is.na(c_wpt_lat)) %>%
      select(!c(m_present_time)) %>%
      format(., nsmall = 4) %>% #coerce to character keeping zeroes out to 4 decimals
      group_by(gliderName) %>%
      slice_tail()  %>% 
      mutate(lat = gliderGPS_to_dd(c_wpt_lat),
             long = gliderGPS_to_dd(c_wpt_lon))
    
    if (nrow(gotoDF) > 0){
      gotoFiles <- gotoDF %>%
        filter(str_ends(fileName, "goto_l10.ma")) %>%
        group_by(gliderName) %>%
        arrange(fileName, .by_group = TRUE)
    }
    
    gotoN <- as.integer(nrow(gotoFiles))
    
    if (gotoN > 0){
      #build goto history
      gotoHistory <- list()
      for (i in 1:gotoN) {
        #glider <- as.character(gotoFiles[i,1])
        gotoHistory[[as.character(gotoFiles[i,1])]] <- gotoLoad(paste0("/gliders/gliders/", gotoFiles[i,1], "/archive/", gotoFiles[i,2]))
      }
      
      #get most recent goto file
      gotoHistDF <- bind_rows(gotoHistory, .id = "gliderName")
      
    }
    
    goto <- cwpt %>%
      # group_by(gliderName) %>%
      # arrange() %>%
      # slice_tail() %>%
      mutate(val = 2) %>%
      full_join(endDF %>%
                  mutate(val = 1)) %>%
      arrange(val, .by_group = TRUE) %>%
      st_as_sf(coords = c("long", "lat"))  %>%
      group_by(gliderName) %>% 
      mutate(seg_end = lead(geometry)) %>%
      rowwise() %>%
      mutate(geometry = st_union(geometry, seg_end) %>% st_cast("LINESTRING")) %>%
      ungroup() %>%
      select(!starts_with("seg"))
    
    liveMissionMap <- leaflet(mapSF) %>%
      #base provider layers
      addWMSTiles("https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}.png",
                  layers = "World_Ocean_Base",
                  group = "Ocean Basemap",
                  options = WMSTileOptions(format = "image/png", transparent = F)) %>%
      addWMSTiles("https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Reference/MapServer/tile/{z}/{y}/{x}.png",
                  layers = "World_Ocean_Reference",
                  group = "Ocean Reference",
                  options = WMSTileOptions(format = "image/png", transparent = T)) %>%
      addWMSTiles("https://www.gebco.net/data_and_products/gebco_web_services/web_map_service/mapserv?",
                  layers = "GEBCO_LATEST",
                  group = "GEBCO",
                  options = WMSTileOptions(format = "image/png", transparent = F)) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "World Imagery") %>%
      addLayersControl(baseGroups = c('Ocean Basemap', 'GEBCO', 'World Imagery'),
                       overlayGroups = c('Ocean Reference')) %>%
      setView(lat = 27.75, lng = -83, zoom = 8) %>%
      addPolylines(
        color = "grey",
        weight = 3,
        opacity = 1,
      ) %>%
      #timestamps for surfacings
      addCircles(data = mapDF,
                 lat = mapDF$lat,
                 lng = mapDF$long,
                 color = ~pal(mapDF$gliderName),
                 popup = mapDF$m_present_time,
                 weight = 3
      ) %>%
      #start marker
      # addAwesomeMarkers(data = startDF,
      #                   lat = ~lat,
      #                   lng = ~long,
      #                   label = "Initial position",
      #                   icon = icon.start
      # ) %>%
      #end marker
      addAwesomeMarkers(data = endDF,
                        lat = ~lat,
                        lng = ~long,
                        label = "Latest position",
                        icon = icon.latest
      ) %>%
      addMeasure(primaryLengthUnit = "kilometers",
                 secondaryLengthUnit = "miles") %>%
      addSimpleGraticule(interval = 2.5) %>%
      addPolygons(data = rhombus,
                  lat = rhombus$latitude,
                  lng = rhombus$longitude)
    # addCircles(data = serialRead(),
    #            lat = serialRead()$latitude,
    #            lng = serialRead()$longitude,
    #            color = "purple",
    #            weight = 20,
    #            opacity = 0.5)
    
    if (nrow(cwpt > 0)) {
      liveMissionMap <- liveMissionMap %>%
        addMarkers(lat = ~cwpt$lat,
                   lng = ~cwpt$long,
                   label = "Commanded wpt") %>%
        addArrowhead(data = goto, color="blue",
                     options = arrowheadOptions(
                       #yawn = 60,
                       size = '10%',
                       frequency = 'allvertices',
                       fill = TRUE,
                       opacity=0.5, stroke=TRUE, fillOpacity=0.4,
                       proportionalToTotal = TRUE,
                       offsets = NULL,
                       perArrowheadOptions = NULL))
    }
    
    # if (gotoN > 0) {
    #   liveMissionMap <- liveMissionMap %>%
    #     addCircles(lat = goto$lat,
    #                lng = goto$long,
    #                radius = goto$rad,
    #                label = goto$comment) %>%
    # addArrowhead(lat = goto$lat,
    #              lng = goto$long, color="blue",
    #              options = arrowheadOptions(
    #                #yawn = 60,
    #                size = '10%',
    #                frequency = 'allvertices',
    #                fill = TRUE,
    #                opacity=0.5, stroke=TRUE, fillOpacity=0.4,
    #                proportionalToTotal = TRUE,
    #                offsets = NULL,
    #                perArrowheadOptions = NULL))
    # }
    #setView(lat = 27.75, lng = -83, zoom = 6)
    
    liveMissionMap
    
  })
  
  observe({
    shipIcon <- makeAwesomeIcon(icon = "arrow-up",
                                iconRotate = vtg()$headingT,
                                squareMarker = FALSE,
                                markerColor = "black")
    
    
    
    test <- gps() %>%
      mutate(speedN = as.numeric(vtg()$speedN),
             headingT = as.numeric(hdg()$heading))
    
    #calculate new position
    future <- calculate_new_gps_position(test)
    
    leafletProxy("missionmapLive") %>%
      removeMarker("ship") %>%
      removeArrowhead("ship") %>%
      # addAwesomeMarkers(lng = gps()$longitude, 
      #                   lat = gps()$latitude, 
      #                   layerId = "ship",
      #                   icon = shipIcon, 
      #                   popup = "Hogarth") %>%
      addCircleMarkers(data = gps(),
                       lat = gps()$latitude,
                       lng = gps()$longitude,
                       layerId = "ship",
                       color = "red",
                       weight = 9,
                       opacity = 0.5,
                       popup = "Hogarth") %>%
      addArrowhead(lat = future$latitude,
                   lng = future$longitude, 
                   layerId = "ship",
                   color="red",
                   options = arrowheadOptions(
                     #yawn = 60,
                     size = '22%',
                     frequency = 'allvertices',
                     fill = TRUE,
                     opacity=0.5, stroke=TRUE, fillOpacity=0.4,
                     proportionalToTotal = TRUE,
                     offsets = NULL,
                     perArrowheadOptions = NULL))
    
  })
  
  # session$onSessionEnded(function() {
  #   
  #   close(conn)
  # })
  
}