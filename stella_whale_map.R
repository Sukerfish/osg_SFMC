library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
library(leaflet.extras2)
library(crepuscule)

#bounding box setup
latitude <- as.numeric(c("24", "34", "34", "24"))
longitude <- as.numeric(c("-81.5", "-81.5", "-87", "-87"))
rhombus <- data.frame(latitude, longitude)
eGOM <- rhombus %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4008) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
# 
#load in full isobaths and select
eyeso <- st_read("./isobaths/w98e78n31s18_isobath_5-100m.shp")
eyeso <- st_read("./isobaths/w98e78n31s18_isobath_100-1000m.shp")

iso200 <- eyeso %>%
  filter(CONTOUR %in% c(-100,-200,-300)) %>%
  st_intersection(eGOM)

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

load(paste0("./thebrewery/Data/M125_usf-stellaNoEK.RData"))

glider_track <- gliderdf %>%
  select(i_lon, i_lat, m_present_time) %>%
  filter(!is.na(i_lat)) %>%
  arrange(m_present_time) %>%
  slice(which(row_number() %% 10 == 1)) %>%
  select(i_lon, i_lat)

glider_sf <- st_linestring(as.matrix(glider_track), dim = "XY")

raw_list <- readxl::read_excel("detections.xlsx", col_names = FALSE)
raw_list2 <- readxl::read_excel("detections_old.xlsx", col_names = FALSE)

whale_times <- raw_list %>%
  select(last_col()) %>%
  rename(timestamps = last_col()) %>%
  mutate(time_det = parse_date_time(timestamps, "ymdHMS")) %>%
  distinct()

whale_times2 <- raw_list2 %>%
  select(last_col()) %>%
  rename(timestamps = last_col()) %>%
  mutate(time_det = parse_date_time(timestamps, "ymdHMS")) %>%
  distinct()

test <- setdiff(whale_times$timestamps, whale_times2$timestamps)

#loop through detection list
out <- list()
for (i in 1:nrow(whale_times)) {
  hold <- data.frame()
  hold <- gliderdf %>%
    slice(which.min(abs(as.numeric(m_present_time) - as.numeric(c(whale_times[i,2])))))
  
  out[[i]] <- hold
  
}

whale_locations <- bind_rows(out, .id = "det_number")

whale_crep <- whale_locations %>%
  select(m_present_time, i_lat, i_lon) %>%
  rename(dateTime = m_present_time,
         latitude = i_lat,
         longitude = i_lon)

test <- get_sun_events(whale_crep) %>%
  mutate(timing = ifelse(dateTime < sunrise | dateTime > sunset, "night", "day")) %>%
  mutate(timeH = hour(dateTime))

(whale_light <- ggplot(data = test, aes(x = timeH, fill = timing)) +
  geom_bar() +
    scale_fill_manual(values = c("orange", "black")) +
    labs(title = "Day or Night Call Detections",
         x = "Hour (UTC)"))


whale_full <- gliderdf %>%
  filter(yo_id %in% c(whale_locations$yo_id)) %>%
  group_by(yo_id) %>%
  mutate(yo_lat = mean(i_lat, na.rm = TRUE),
         yo_lon = mean(i_lon, na.rm = TRUE))

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
  addPolylines(data = glider_sf,
               weight = 2,
               opacity = .8) %>%
  addPolylines(data = iso200,
               weight = 2,
               color= "black") %>%
  addCircles(data = whale_locations,
             lat = whale_locations$i_lat,
             lng = whale_locations$i_lon,
             color = "black",
             popup = whale_locations$m_present_time
  ) %>%
  addAwesomeMarkers(
    lat = glider_sf[1, 2],
    lng = glider_sf[1, 1],
    label = "Initial position",
    icon = icon.start
  ) %>%
  #end marker
  addAwesomeMarkers(
    lat = glider_sf[nrow(glider_sf), 2],
    lng = glider_sf[nrow(glider_sf), 1],
    label = "End position",
    icon = icon.end
  ) %>%
  setView(lng = mean(whale_locations$i_lon),
          lat = mean(whale_locations$i_lat),
          zoom = 7)
