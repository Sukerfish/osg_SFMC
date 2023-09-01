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
#library(echogram)
#library(nmea)

source("./scripts/ssv_to_df.R")
source("./scripts/loadSSV.R")
source("./scripts/pseudogram.R")
source("./scripts/gotoLoad.R")
source("./scripts/gliderGPS_to_dd.R")

getGPRMC <- function(data) {
  ans <- list(rmc=NULL, rest=data)
  rxp <- "\\$GPRMC(,[^,]*){12}\\*[0-9,A-F]{2}\r\n"
  beg <- regexpr(rxp, data)
  if(beg == -1) return(ans)
  end <- beg + attr(beg, "match.length")
  sub <- substr(data, beg, end - 6)
  ans$rmc <- strsplit(sub, ",")[[1]]
  names(ans$rmc) <- c("id","utc","status","lat","N/S",
                      "long","E/W","knots","cog","date",
                      "mag","E/W","mode")
  ans$rest <- substr(data, end, nchar(data))
  return(ans)
}

getAllGPRMC <- function(data) {
  res <- getGPRMC(data)
  ans <- res$rmc
  while(!is.null(res$rmc)) {
    ans <- rbind(ans, res$rmc)
    res <- getGPRMC(res$rest)
  }
  return(ans)
}

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

# conn <- serialConnection("arduino", port="com4", mode="9600,n,8,1")
# open(conn)
# close(conn)

ui <-
  dashboardPage(
    dashboardHeader(title = "Glider V Boat"),
    dashboardSidebar(
      numericInput(inputId = "hours",
                   label = "how many hours",
                   value = 12,
                   min = 1,
                   max = Inf
      ),
      checkboxGroupInput(inputId = "gliders",
                         label = "which gliders",
                         choices = NULL,
                         selected = NULL)
    ),
    dashboardBody(
      box(
      #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("missionmapLive", height=575),
      width = 12,
      height = 600
    )
    )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  serialRead <- reactive({
    invalidateLater(100, session)
    read.serialConnection(conn)
  })
  
  output$serial <- renderText({serialRead()})
  
  testdata <-
    "5801,W,1,04,2.1,57.7,M,-32.3,M,,*4E\r\n$GPGSA,A,2,03,06,,16,23,,,,,,,,2.3,2.1,1.0*33\r\n$GPGSV,2,2,07,23,71,280,43,24,00,084,00,31,13,096,00*41\r\n$GPGLL,3245.3949,N,07955.4646,W,230318,A,A*56\r\n$GPBOD,,T,,M,,*47\r\n$GPVTG,0.0,T,7.2,M,0.0,N,0.0,K*4B\r\n$PGRME,11.5,M,40.8,M,42.4,M*15\r\n$PGRMZ,189,f,2*1A\r\n$PGRMM,WGS 84*06\r\n$GPRTE,1,1,c,*37\r\n$GPRMC,230320,A,2745.1080,N,08355.4646,W,0.0,0.0,110510,7.2,W,A*1C\r\n$GPRMB,A,,,,,,,,,,,,A,A*0B\r\n$GPGGA,230320,3246.1080,N,07955.4646,W,1,04,2.1,59.8,M,-32.3,M,,*4A\r\n$GPGSA,A,2,03,06,,16,23,,,,,,,,2.3,2.1,1.0*33\r\n$GPGSV,2,1,07,03,60,147,45,06,59,113,41,07,14,299,00,16,46,036,45*76\r\n$GPGLL,3246.1080,N,07955.4646,W,230320,A,A*53\r\n$GPBOD,,T,,M,,*47\r\n$GPVTG,0.0,T,7.2,M,0.0,N,0.0,K*4B\r\n$PGRME,11.5,M,40.8,M,42.4,M*15\r\n$PGRMZ,196,f,2*14\r\n$PGRMM,WGS 84*06\r\n$GPRTE,1,1,c,*37\r\n$GPRMC,230322,A,3246.1080,N,07955.4646,W,0.0,0.0,110510,7.2,W,A*13\r\n$GPRMB,A,,,,,,,,,,,,A,A*0B\r\n$GPGGA,230322,3246.1080,N,07955.4646,W,1,04,2.1,57.4,M,-32.3,M,,*47\r\n$GPGSA,A,2,03,06,,16,23,,,,,,,,2.3,2.1,1.0*33\r\n$GPGSV,2,2,07,23,71,280,43,24,00,084,00,31,13,096,00*41\r\n$GPGLL,3246.1080,N,07955.4646,W,230322,A,A*5C\r\n$GPBOD,,T,,M,,*47\r\n$GPVTG,0.0,T,7.2,M,0.0,N,0.0,K*4B\r\n$PGRME,11.5,M,40.8,M,42.4,M*15\r\n$PGRMZ,188,f,2*1B\r\n$PGRMM,WGS 84*06\r\n$GPRTE,1,1,c,*37\r\n$GPRMC,230324,A,3246.1080,N,07955.4646,W,0.0,0.0,110510,7.2,W,A*12\r\n$GPRMB,A,,,,,,,,,,,,A,A*0B\r\n$GPGGA,230324,3246.1080,N,07955.4646,W,1,04,2.1,55.0,M,-32.3,M,,*40\r\n$GPGSA,A,2,03,06,,16,23,,,,,,,,2.3,2.1,1.0*33\r\n$GPGSV,2,1,07,03,60,147,45,06,59,113,41,07,14,299,00,16,46,036,44*77\r\n$GPGLL,3246.1080,N,07955.4646,W,230324,A,A*5D\r\n$GPBOD,,T,,M,,*47\r\n$GPVTG,0.0,T,7.2,M,0.0,N,0.0,K*4B\r\n$PGRME,11.5,M,40.8,M,42.4,M*15\r\n$PGRMZ,180,f,2*13\r\n$PGRMM,WGS 84*06\r\n$GPRTE,1,1,c,*37\r\n$GPRMC,230326,A,3246.1080,N,07955.4646,W,0.0,0.0,110510,7.2,W,A*16\r\n$GPRMB,A,,,,,,,,,,,,A,A*0B\r\n$GPGGA,230326,3246.1080,N,07955.4646,W,1,04,2.1,54.4,M,-32.3,M,,*41\r\n$GPGSA,A,2,03,06,,16,23,,,,,,,,2.3,2.1,1.0*33\r\n$GPGSV,2,2,07,23,71,280,43,24,00,084,00,31,13,096,00*41\r\n$GPGLL,3246.1080,N,07955.4646,W,230326,A,A*59\r\n$GPBOD,,T,,M,,*47\r\n$GPVTG,0.0,T,7.2,M,0.0,N,0.0,K*4B\r\n$PGRME,11.5,M,40.8,M,42.4,M*15\r\n$PGRMZ,179,f,2*15\r\n$PGRMM,WGS 84*06\r\n$GPRTE,1,1,c,*37\r\n$GPRMC,230328,A,3246.1080,N,07955.4646,W,0.0,0.0,110510,7.2,W,A*11\r\n$GPRMB,A,,,,,,,,,,,,A,A*0B\r\n$GPGGA,230328,3246.1080,N,07955.4646,W,1,04,2.1,57.0,M,-32.3,M,,*41\r\n$GPGSA,A,2,03,06,,16,23,,,,,,,,2.3,2.1,1.0*33\r\n$GPGSV,2,1,07,03,60,147,45,06,59,112,41,07,14,299,39,16,46,036,45*7D\r\n$GPGLL,3246.1080,N,07955.4646,W,230328,A,A*5E\r\n$GPBOD,,T,,M,,*47\r\n$GPVTG,0.0,T,7.2,M,0.0,N,0.0,K*4B\r\n$PGR"
  
  test2 <- as.data.frame(getAllGPRMC(testdata)) %>%
    select(c(utc, lat, long, 7))
  
    colnames(test2)[4] = "side"

  test3 <- test2 %>%
    slice_head() %>%
    mutate(latt = as.numeric(lat),
           longg = as.numeric(long)) %>%
    mutate(longgg = ifelse(side == "W", longg*-1, longg)) %>%
    mutate(lat = gliderGPS_to_dd(latt),
           long = gliderGPS_to_dd(longgg))
  
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
      addSimpleGraticule(interval = 2.5) %>%
      addCircles(data = test3,
                 lat = test3$lat,
                 lng = test3$long,
                 color = "purple",
                 weight = 20,
                 opacity = 0.5)
    
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
    
    liveMissionMap
    
    })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
