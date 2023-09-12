library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggtext)
library(patchwork)
library(egg)
library(geosphere)
library(ggrepel)
library(leaflet)
library(gganimate)
library(rayshader)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(spacetime)
library(sp)

source("./COMIT/profilePlot.R")

theme_osg <- function(){ 
  theme_bw() %+replace%
    theme(
      plot.title = element_text(hjust = 0,
                                size = 32),
      plot.subtitle = element_text(hjust = 0,
                                   size = 16),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12),
      plot.caption = element_markdown(hjust = 1),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    )
}

latitude <- as.numeric(c("27.7938", "27.8287", "27.86", "27.8253"))
longitude <- as.numeric(c("-84.2426", "-84.2542", "-84.1608", "-84.1542"))
rhombus <- data.frame(latitude, longitude)
rhomSF <- rhombus %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

#### pull in main data ####
sam <- new.env()
stella <- new.env()

#load live data files in environments
load(paste0("./COMIT/glider_live_sam.RData"), envir = sam)
load(paste0("./COMIT/glider_live_stella.RData"), envir = stella)

#establish time frame of interest
startSam <- ymd_hms("2023/09/03 01:17:00", tz = "UTC")
endSam <- ymd_hms("2023/09/05 02:18:00", tz = "UTC")

startStella <- ymd_hms("2023/09/03 05:41:00", tz = "UTC")
endStella <- ymd_hms("2023/09/04 13:19:00", tz = "UTC")

#grab data from specified window
samdf <- sam$gliderdf %>%
  filter(m_present_time %within% interval(startSam, endSam)) %>%
  mutate(gliderName = "usf-sam")

stelladf <- stella$gliderdf %>%
  filter(m_present_time %within% interval(startStella, endStella)) %>%
  mutate(gliderName = "usf-stella")
#filter(sci_water_cond == 3)
#select(sci_water_cond, sci_water_temp, sci_water_pressure, osg_soundvel1)

#join together keeping all samples from both gliders
fulldf <- stelladf %>%
  full_join(samdf)

svdf <- fulldf %>%
  filter(sci_water_cond > 3) %>%
  filter(sci_water_temp > 0)

minSV <- min(svdf$osg_soundvel1, na.rm = TRUE)
maxSV <- max(svdf$osg_soundvel1, na.rm = TRUE)



##### spacetime #####
tdf <- fulldf %>%
  filter(!is.na(i_lat))
timedf <- tdf %>%
  select(m_present_time)
 time <- as.POSIXct(c(timedf))
spdf <- tdf %>%
  select(i_lat, i_lon) %>%
  filter(!is.na(i_lat)) %>%
  SpatialPoints()
ddf <- tdf %>%
  select(!c(m_present_time, i_lat, i_lon))

test <- stConstruct(tdf, c("i_lat", "i_lon"), "m_present_time")

out <- stplot(test, "osg_soundvel1", mode = "ts")






####### mapping ####
glidePos <- fulldf %>%
  #select(gliderName, segment, yo_id, cast, i_lat, i_lon, osg_i_depth, m_present_time) %>%
  filter(cast != "Surface") %>%
  filter(cast == "Downcast") %>%
  group_by(gliderName, yo_id) %>%
  summarise(profTime = mean(m_present_time, na.rm = TRUE),
            profLat = mean(i_lat, na.rm = TRUE),
            profLon = mean(i_lon, na.rm = TRUE),
            profDepth = max(osg_i_depth, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(profTime) %>%
  mutate(yoFloorTime = floor_date(profTime, "1 second"),
         profID = row_number())

# ideal = expand_grid(
#   id = unique(glidePos$profID),
#   date = seq.Time(from = min(glidePos$profTime), to = max(glidePos$profTime), by = 1)
# )
  

#   grp = sample(4L, 1e3, TRUE)
# )
# DT[ , v := rnorm(.N, sd = 1/.BY$grp), by = grp]
# DT[ , grpI := seq_len(.N), by = grp]
# world <- ne_countries(scale = "medium", returnclass = "sf")
# oceans10 <- ne_download(type = "ocean", scale = 10, category = "physical",
#                         returnclass = "sf")
#bg <- oceans10(scale = "medium")

p <- ggplot() +
  geom_sf(data = world) +
  geom_sf(data = rhomSF) +
  coord_sf(xlim = range(glidePos$profLon, na.rm = TRUE), 
           ylim = range(glidePos$profLat, na.rm = TRUE), 
           expand = TRUE)+
  
  geom_path(data = glidePos, 
            aes(x = profLon, y = profLat, color = gliderName),
            alpha = 0.3) +
  geom_point(data = glidePos, 
             aes(x = profLon, y = profLat, color = gliderName),
             alpha = 0.7, 
             shape = 16, 
             size = 3) +
  geom_point(data = glidePos,
             aes(x = profLon, y = profLat, color = gliderName, group = profID), 
             alpha = 0.7, 
             shape = 21, 
             size = 3) +
  scale_color_manual(values = c("darkgreen", "purple", "black")) +
  #scale_fill_viridis_c(option = "inferno") +
  #scale_color_viridis_d(option = "inferno") +
  scale_size_continuous(range = c(0.1,10)) +
  theme_osg() 

anim = p + 
  #transition_reveal(along = yoFloorTime)+
  transition_reveal(along = profID) +
  #ease_aes('linear') +
  labs(
    title = "Glider profiles over time",
    subtitle = 'Time of profile: {floor_date(frame_along, "1 second")} UTC',
    #subtitle = 'Frame {frame} of {nframes}'
    #color = "Time (UTC)",
    color = "Glider name",
    y = "Latitude",
    x = "Longitude"
  ) +
  theme(plot.title = element_text(vjust = 1.5),
        plot.subtitle = element_text(vjust = 1.5),
        strip.text.x = element_text(size = 16),
        strip.background.x = element_blank())

animate(anim, height = 800, width = 800, end_pause = 30)
anim_save(filename = "allProfiles.gif",
          animation = anim,
          #device = "png",
          path = "./COMIT",
          width = 800,
          height = 800,
          end_pause = 15)

pal <- colorFactor("PuOr", test$gliderName)

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
  addPolygons(data = rhombus,
              lat = rhombus$latitude,
              lng = rhombus$longitude,
              opacity = 0.5) %>%
  setView(lat = 27.82, lng = -84.2, zoom = 12) %>%
  addCircleMarkers(data = test,
                   lat = test$i_lat_m,
                   lng = test$i_lon_m,
                   color = ~pal(test$gliderName),
                   radius = 10,
                   stroke = FALSE,
                   fillOpacity = 1
  ) %>%
  addCircleMarkers(data = shipLat,
                   lat = shipLat$sLat,
                   lng = shipLat$sLon,
                   color = "red",
                   radius = 10,
                   stroke = FALSE,
                   fillOpacity = 1
  )
