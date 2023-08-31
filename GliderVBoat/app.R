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
library(leaflet)
library(leaflet.extras2)
library(serial)

source("./scripts/ssv_to_df.R")
source("./scripts/loadSSV.R")
source("./scripts/pseudogram.R")
source("./scripts/gotoLoad.R")
source("./scripts/gliderGPS_to_dd.R")

# conn <- serialConnection("arduino", port="ttyUSB3", mode="9600,n,8,1")
# open(conn)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Glider V Boat"),

    # Sidebar with a slider input for number of bins 
    box(
      leafletOutput(outputId = "missionmapLive")
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  serialRead <- reactive({
    invalidateLater(100, session)
    read.serialConnection(conn)
  })
  
  output$serial <- renderText({serialRead()})
  
  
  
  autoInvalidate <- reactiveTimer(3000, NULL)
  
  load(paste0("/echos/", "usf-gansett", "/glider_live.RData"))
  
  #live mission map
  #massage gps data a lot
  map_sf <- gliderdf %>%
    select(m_present_time, m_gps_lon, m_gps_lat) %>%
    filter(!is.na(m_gps_lat)) %>%
    mutate(latt = format(m_gps_lat, nsmall = 4),
           longg = format(m_gps_lon, nsmall = 4)) %>% #coerce to character keeping zeroes out to 4 decimals
    mutate(lat = gliderGPS_to_dd(latt),
           long = gliderGPS_to_dd(longg)) %>%
    filter(lat >= -90 & lat <= 90) %>% #remove illegal values
    filter(long >= -180 & long <= 180)
  
  # if (nrow(toGliderList) > 0){
  gotoFiles <- toGliderList %>%
    filter(str_ends(fileName, "goto_l10.ma")) %>%
    arrange(fileName)
  
  #get commanded wpt
  cwpt <- gliderdf %>%
    select(m_present_time, c_wpt_lat, c_wpt_lon) %>%
    filter(!is.na(c_wpt_lat)) %>%
    select(!c(m_present_time)) %>%
    format(., nsmall = 4) %>% #coerce to character keeping zeroes out to 4 decimals
    tail(1)  %>% 
    mutate(lat = gliderGPS_to_dd(c_wpt_lat),
           long = gliderGPS_to_dd(c_wpt_lon))
  
  gotoN <- as.integer(nrow(gotoFiles))
  
  if (gotoN > 0){
    #build goto history
    gotoHistory <- list()
    for (i in 1:gotoN) {
      gotoHistory[[i]] <- gotoLoad(paste0("/gliders/gliders/", "usf-gansett", "/archive/", gotoFiles[i,]))
    }
    
    #get most recent goto file
    goto <- as.data.frame(tail(gotoHistory, 1))
  }
  
  liveMissionMap <- leaflet() %>%
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
      lat = map_sf$lat,
      lng = map_sf$long,
      color = "grey",
      weight = 3,
      opacity = 1,
    ) %>%
    #timestamps for surfacings
    addCircles(data = map_sf,
               lat = map_sf$lat,
               lng = map_sf$long,
               color = "gold",
               popup = map_sf$m_present_time,
               weight = 3
    ) %>%
    #start marker
    addMarkers(
      lat = map_sf[1, "lat"],
      lng = map_sf[1, "long"],
      label = "Initial position"
    ) %>%
    #end marker
    addMarkers(
      lat = map_sf[nrow(map_sf), "lat"],
      lng = map_sf[nrow(map_sf), "long"],
      label = "Latest position"
    ) %>%
    addMeasure(primaryLengthUnit = "kilometers",
               secondaryLengthUnit = "miles") %>%
    addSimpleGraticule(interval = 2.5)
  
  if (nrow(cwpt > 0)) {
    liveMissionMap <- liveMissionMap %>%
      addMarkers(lat = cwpt$lat,
                 lng = cwpt$long,
                 label = "Commanded wpt")
  }
  
  if (gotoN > 0) {
    liveMissionMap <- liveMissionMap %>%
      addCircles(lat = goto$lat,
                 lng = goto$long,
                 radius = goto$rad,
                 label = goto$comment) %>%
      addArrowhead(lat = goto$lat,
                   lng = goto$long, color="blue",
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
  #setView(lat = 27.75, lng = -83, zoom = 6)
  
  output$missionmapLive <- renderLeaflet({
    liveMissionMap})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
