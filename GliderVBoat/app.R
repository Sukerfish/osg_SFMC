#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras2)
library(serial)
library(sf)

source("./scripts/ssv_to_df.R")
source("./scripts/loadSSV.R")
source("./scripts/pseudogram.R")
source("./scripts/gotoLoad.R")
source("./scripts/gliderGPS_to_dd.R")

icon.start <- makeAwesomeIcon(
  icon = "flag", markerColor = "green",
  library = "fa",
  iconColor = "black"
)

icon.end <- makeAwesomeIcon(
  icon = "flag", markerColor = "red",
  library = "fa",
  iconColor = "black"
)

icon.latest <- makeAwesomeIcon(
  icon = "flag", markerColor = "purple",
  library = "fa",
  iconColor = "black"
)

# conn <- serialConnection("arduino", port="ttyUSB3", mode="9600,n,8,1")
# open(conn)

# Define UI for application that draws a histogram
ui <- fillPage(

    # Application title
    titlePanel("Glider V Boat"),
    
      leafletOutput(outputId = "missionmapLive", 
                    height = "100%")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  serialRead <- reactive({
    invalidateLater(100, session)
    read.serialConnection(conn)
  })
  
  output$serial <- renderText({serialRead()})
  
  
  
  autoInvalidate <- reactiveTimer(3000, NULL)
  
  #get deployed gliders
  deployedGliders <- read.csv("/echos/deployedGliders.txt", 
                              sep = "",
                              header = FALSE)
  colnames(deployedGliders)[1] = "Name"
  colnames(deployedGliders)[2] = "ahrCap"
  
  #only process "real" ones
  deployedGliders <- deployedGliders %>%
    filter(!str_starts(Name,"#")) #remove any commented lines
  
  
  mapList <- list()
  for (i in deployedGliders$Name){

    glide <- new.env()
    #load latest live data file
    load(paste0("/echos/", i, "/glider_live.RData"), envir = glide)
    
    mapList[[i]] <- glide$gliderdf
    
  }
  
  #fullDF <- data.frame() #initialize
  fullDF <- bind_rows(mapList, .id = "gliderName")
  
  
  gliderDay <- interval(max(fullDF$m_present_time), #lower bound
                         max(fullDF$m_present_time) - hours(24)  #upper bound
  ) 
  
###### live mission map #########
  
  #massage gps data a lot
    mapDF <- fullDF %>%
    select(m_present_time, m_gps_lon, m_gps_lat, gliderName) %>%
    filter(!is.na(m_gps_lat)) %>%
    filter(m_present_time %within% gliderDay) %>%
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
  
  # if (nrow(toGliderList) > 0){
  # gotoFiles <- toGliderList %>%
  #   filter(str_ends(fileName, "goto_l10.ma")) %>%
  #   arrange(fileName)
  
  #get commanded wpt
  cwpt <- fullDF %>%
    select(m_present_time, c_wpt_lat, c_wpt_lon, gliderName) %>%
    filter(m_present_time %within% gliderDay) %>%
    filter(!is.na(c_wpt_lat)) %>%
    select(!c(m_present_time)) %>%
    format(., nsmall = 4) %>% #coerce to character keeping zeroes out to 4 decimals
    group_by(gliderName) %>%
    slice_tail()  %>% 
    mutate(lat = gliderGPS_to_dd(c_wpt_lat),
           long = gliderGPS_to_dd(c_wpt_lon))
  
  # gotoN <- as.integer(nrow(gotoFiles))
  
  # if (gotoN > 0){
  #   #build goto history
  #   gotoHistory <- list()
  #   for (i in 1:gotoN) {
  #     gotoHistory[[i]] <- gotoLoad(paste0("/gliders/gliders/", "usf-gansett", "/archive/", gotoFiles[i,]))
  #   }
  #   
  #   #get most recent goto file
  #   goto <- as.data.frame(tail(gotoHistory, 1))
  # }

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
    addAwesomeMarkers(data = startDF,
      lat = ~lat,
      lng = ~long,
      label = "Initial position",
      icon = icon.start
    ) %>%
    #end marker
    addAwesomeMarkers(data = endDF,
               lat = ~lat,
               lng = ~long,
      label = "Latest position",
      icon = icon.latest
    ) %>%
    addMeasure(primaryLengthUnit = "kilometers",
               secondaryLengthUnit = "miles") %>%
    addSimpleGraticule(interval = 2.5)

  if (nrow(cwpt > 0)) {
    liveMissionMap <- liveMissionMap %>%
      addMarkers(lat = ~cwpt$lat,
                 lng = ~cwpt$long,
                 label = "Commanded wpt")
  }

  # if (gotoN > 0) {
  #   liveMissionMap <- liveMissionMap %>%
  #     addCircles(lat = goto$lat,
  #                lng = goto$long,
  #                radius = goto$rad,
  #                label = goto$comment) %>%
  #     addArrowhead(lat = goto$lat,
  #                  lng = goto$long, color="blue",
  #                  options = arrowheadOptions(
  #                    #yawn = 60,
  #                    size = '10%',
  #                    frequency = 'allvertices',
  #                    fill = TRUE,
  #                    opacity=0.5, stroke=TRUE, fillOpacity=0.4,
  #                    proportionalToTotal = TRUE,
  #                    offsets = NULL,
  #                    perArrowheadOptions = NULL))
  # }
  #setView(lat = 27.75, lng = -83, zoom = 6)
  
  output$missionmapLive <- renderLeaflet({
    liveMissionMap})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
