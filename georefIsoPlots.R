
library(tidyverse)
library(lubridate)
# library(leaflet)
# library(leaflet.extras2)
# library(htmlwidgets)
library(osgUtils)
# library(terra)
library(ggplot2)
library(cmocean)
library(viridis)
library(patchwork)

library(reshape2)
library(MBA)
library(mgcv)
library(marmap)
library(FNN)

library(leaflet)
library(leaflet.extras2)

library(sf)
library(geosphere)
# library(lwgeom)
# library(units)
#library(htmlwidgets)

missionList <- c(
  #"M122_usf-bass",
  # "M123_usf-jaialai",
  # "M125_usf-stellaNoEK",
  # "M126_usf-gansett",
  # "M127_usf-jaialai",
  # "M129_usf-gansett",
  # "M130_usf-jaialai",
  # "M131_usf-jaialai",
  # "M132_usf-gansett",
  # "M133_usf-sam",
  # "M136_usf-sam",
  "M137_usf-jaialai"
) %>%
  sort()

#bounding box setup
latitude <- as.numeric(c("25.5", "31", "31", "25.5"))
longitude <- as.numeric(c("-81.5", "-81.5", "-88", "-88"))
rhombus <- data.frame(latitude, longitude)
eGOM <- rhombus %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4008) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

#load in full isobaths and select
eyeso <- st_read("./isobaths/w98e78n31s18_isobath_5-100m.shp")

iso30 <- eyeso %>%
  filter(CONTOUR == -30) %>%
  st_intersection(eGOM)

iso50 <- eyeso %>%
  filter(CONTOUR == -50) %>%
  st_intersection(eGOM)

iso100 <- eyeso %>%
  filter(CONTOUR == -100) %>%
  st_intersection(eGOM)

#check
# ggplot() +
#   geom_sf(data = eGOM) +
#   geom_sf(data = iso30) +
#   geom_sf(data = iso50) +
#   geom_sf(data = iso100)

depth <- c(-30, -50, -100)

output <- list()
for (i in missionList){
  print(i) #watch progress
  holding <- new.env() #clear environment each time
  
  load(paste0("./thebrewery/Data/", i, ".RData"), envir = holding) #load in
  
  #process
  depthDF <- holding$gliderdf %>%
    mutate(ddate = date(m_present_time)) %>%
    group_by(yo_id) %>%
    mutate(gDepth = max(osg_i_depth, na.rm = TRUE),
           wDepth = max(m_water_depth, na.rm = TRUE)) %>%
    filter(m_present_time %within% interval(ymd("2023-01-01"), ymd("2023-12-31"))) %>%
    #ungroup() %>%
    arrange(m_present_time, .by_group = FALSE)
  
  gliderTrack <- depthDF %>%
    filter(!is.na(i_lon)) %>%
    filter(!is.na(yo_id)) %>%
    arrange(m_present_time) %>%
    group_by(yo_id) %>% 
    slice(c(1,n())) %>%
    st_as_sf(coords = c("i_lon", "i_lat"), crs = 4008) %>%
    group_by(yo_id) %>%
    dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
    st_cast("LINESTRING")
  
  yoDepth <- depthDF %>%
    distinct(yo_id, gDepth, wDepth) %>%
    filter(yo_id > 0)
  
  #find deepest dive
  deepestYo <- yoDepth$yo_id[which(abs(yoDepth$gDepth - max(yoDepth$gDepth, na.rm = TRUE)) == min(abs(yoDepth$gDepth - max(yoDepth$gDepth, na.rm = TRUE))))]
  
  yos <- list()
  for (j in seq_along(depth)){
    iso <- eyeso %>%
      filter(CONTOUR == depth[j]) %>%
      st_intersection(eGOM)
    
    #add check for nearest?
    targets <- gliderTrack[lengths(st_intersects(gliderTrack, iso)) > 0,] %>%
      distinct(yo_id) %>%
      mutate(transect = ifelse(yo_id < deepestYo, 0, 1)) %>% #0 headed off, 1 headed in
      group_by(transect) %>%
      slice(1)
    
    if(length(targets$yo_id[targets$transect == 0]) > 0){
    outYo <- depthDF %>%
      filter(yo_id == targets$yo_id[targets$transect == 0]) %>%
      filter(cast == "Downcast") %>%
      #drop the data if the glider didn't get within 15m of the target
      #filter(between(gDepth, depth[j]-15, depth[j]+15)) %>%
      mutate(isobath = -1*depth[j]) %>%
      mutate(transect = "out")
    } else {
      outYo <- data.frame()
    }
    
    if(length(targets$yo_id[targets$transect == 1]) > 0){
    inYo <- depthDF %>%
      filter(yo_id == targets$yo_id[targets$transect == 1]) %>%
      filter(cast == "Downcast") %>%
      #drop the data if the glider didn't get within 15m of the target
      #filter(between(gDepth, depth[j]-15, depth[j]+15)) %>%
      mutate(isobath = -1*depth[j]) %>%
      mutate(transect = "in")
    } else {
      inYo <- data.frame()
    }

    yos[[j]] <- bind_rows(outYo, inYo)
    rm(targets, outYo, inYo, iso)
  }
  
  #export
  output[[i]] <- bind_rows(yos)
  rm(depthDF, yoDepth, deepestYo, yos, gliderTrack)
}

isobathDF <- bind_rows(output, .id = "missionID") %>%
  separate(missionID,
           c("missionNum","gliderName"),
           sep = "_",
           remove = FALSE) %>%
  ungroup() %>%
  unite("isoMission",
        c(missionNum, isobath, transect),
        sep = "_",
        remove = FALSE) %>%
  mutate(isobath = as.factor(isobath))

odvVars <- c("sci_water_temp",
             "osg_rho",
             "sci_flbbcd_chlor_units",
             "sci_flbbcd_bb_units")

exportData <- isobathDF %>%
  group_by(missionID, yo_id) %>%
  mutate(yo_lat = mean(i_lat, na.rm = TRUE),
         yo_lon = mean(i_lon, na.rm = TRUE)) %>%
  select(missionID, isoMission, yo_id, isobath, transect, m_present_time, yo_lat, yo_lon, osg_i_depth, any_of(odvVars))

zz <- distinct(exportData, missionID, yo_lat, yo_lon)

leaflet() %>%
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
  #timestamps for surfacings
  addPolylines(data = iso100) %>%
  # addPolylines(data = gliderTrack) %>%
  addCircles(data = zz,
             lat = zz$yo_lat,
             lng = zz$yo_lon,
             color = "black",
             popup = zz$missionID
  ) %>%
  setView(lng = mean(zz$yo_lon),
          lat = mean(zz$yo_lat),
          zoom = 7)
