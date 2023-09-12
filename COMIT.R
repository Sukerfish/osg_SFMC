library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggtext)
library(patchwork)
library(egg)
library(geosphere)
library(ggrepel)
library(leaflet)
library(cmocean)

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

######## processing #######

#GPS location proximity first 
#geographic
df <- fulldf %>%
  filter(m_present_time %within% interval(startStella, endStella)) %>%
  filter(!is.na(i_lon)) %>%
  rename(group = gliderName)

# Sort the data frame by time within each group
df <- df[order(df$group, df$m_present_time),]

# Create a new data frame to store the minimum distances between groups
min_distance_df <- data.frame()

# Iterate over unique time points
unique_time_points <- unique(df$m_present_time)

for (time_point in unique_time_points) {
  # Subset data for the current time point
  time_data <- df[df$m_present_time == time_point, ]
  
  # Ensure there are at least 2 unique groups at this time point
  if (length(unique(time_data$group)) < 2) {
    next
  }
  
  # Create all possible pairs of groups
  group_pairs <- combn(unique(time_data$group), 2)
  
  # Calculate distances between group pairs
  distances <- apply(group_pairs, 2, function(pair) {
    group1_data <- time_data[time_data$group == pair[1], ]
    group2_data <- time_data[time_data$group == pair[2], ]
    
    min_dist <- min(geosphere::distGeo(group1_data[, c("i_lon", "i_lat")], group2_data[, c("i_lon", "i_lat")]))
    
    return(min_dist)
  })
  
  # Find the minimum distance and corresponding group pair
  min_dist <- min(distances)
  closest_pair <- group_pairs[, which.min(distances)]
  
  # Add the result to the min_distance_df data frame
  min_distance_df <- rbind(min_distance_df, data.frame(time_point = time_point, group1 = closest_pair[1], group2 = closest_pair[2], min_distance = min_dist))
}

# manipulate the min_distance_df data frame as needed
outs <- min_distance_df %>%
  filter(min_distance < 1000)

#identify which yo_ids (per glider) are in the closest time idenitfied in above 
yoFind <- fulldf %>%
  group_by(gliderName, yo_id) %>%
  filter(m_present_time %in% as_datetime(max(outs$time_point))) %>%
  select(gliderName, yo_id) %>%
  ungroup()

#extract only the identified yos
yoDF <- fulldf %>%
  filter((gliderName == yoFind[1,]$gliderName & yo_id == yoFind[1,]$yo_id) |
           (gliderName == yoFind[2,]$gliderName & yo_id == yoFind[2,]$yo_id)) %>%
  group_by(gliderName) %>%
  arrange(m_present_time, by_group = TRUE)

shipCast <- read.csv(file = "./COMIT/025514_2023-09-03_11-31-20.csv", header = TRUE, skip = 104)
sLat <- as.numeric(c("27.82"))
sLon <- as.numeric(c("-84.15"))
shipLat <- data.frame(sLat, sLon)

#### plotting ####

plots <- list()
for (i in unique(fulldf$gliderName)){
qf <- fulldf %>%
  filter(sci_water_temp > 0) %>%
  filter(sci_water_cond >3) %>%
  filter(gliderName == i)

wf <- fulldf %>%
  filter(m_water_depth > 0) %>%
  filter(gliderName == i)

plots[[i]] <- 
  ggplot(data = qf,
         aes(x=m_present_time,
             y=osg_i_depth,
             #z=osg_soundvel1
         )) +
  geom_point(
    aes(color = osg_soundvel1)
  ) +
  #geom_hline(yintercept = 0) +
  xlim(startSam, endSam) +
  scale_y_reverse() +
  scale_color_cmocean(name = "speed",
                      limits = c(minSV, maxSV)) +
  # scale_colour_viridis_c(limits = c(minSV, maxSV)) +
  geom_point(data = wf,
             aes(y = m_water_depth),
             size = 0.3,
             color = "black",
             na.rm = TRUE
  ) +
  theme_bw() +
  labs(subtitle = i,
       # caption = "Calculated using Coppens <i>et al.</i> (1981) 
       #         <br>
       #         <br>
       #         <img src='./www/cms_horiz.png' width='200'/>",
       color = "Sound speed (m/s)",
       y = "Depth (m)",
       x = "Date/Time (UTC)") +
  # theme_osg() + 
  geom_vline(xintercept = as_datetime(max(outs$time_point)),
             linewidth = 1,
             color = "red")

}

fullPlot <- wrap_plots(plots) +
  plot_layout(guides = "collect",
              ncol = 1) +
  plot_annotation(title = "COMIT 2023 Glider Sound Speed",
                  caption = "Calculated using Coppens <i>et al.</i> (1981)
               <br>
               <br>
               <img src='./www/cms_horiz.png' width='200'/>"
                  ) &
  theme_osg() & 
  ylab(NULL) & 
  theme(plot.margin = margin(5.5, 5.5, 5.5, 0))

zlot <- wrap_elements(fullPlot) +
  labs(tag = "Depth (m)") +
  theme(
    plot.tag = element_text(size = 16, angle = 90),
    plot.tag.position = "left"
  )
plot(zlot)

# ggsave(filename = "GliderSoundSpeedFullwithLine.png",
#        plot = zlot,
#        device = "png",
#        path = "./COMIT",
#        width = 16,
#        height = 9)

### yo plot
yoPlots <- list()
for (y in unique(yoDF$gliderName)){
  yf <- yoDF %>%
    ungroup() %>%
    filter(gliderName == y)
  
yoPlots[[y]] <- ggplot(data = yf,
                       aes(x=m_present_time,
                           y=osg_i_depth,
                           #z=osg_soundvel1
                       )) +
  geom_point(
    aes(color = osg_soundvel1)
  ) +
  #geom_hline(yintercept = 0) +
  xlim(min(yoDF$m_present_time, na.rm = TRUE), max(yoDF$m_present_time, na.rm = TRUE)) +
  scale_y_reverse() +
  scale_colour_viridis_c(limits = c(minSV, maxSV)) +
  geom_point(data = wf,
             aes(y = m_water_depth),
             size = 0.3,
             color = "black",
             na.rm = TRUE
  ) +
  labs(subtitle = y,
       y = "Depth (m)",
       x = "Date") +
  theme_osg()
  #geom_vline(xintercept = as_datetime(max(outs$time_point)))

}

yoPlot <- wrap_plots(yoPlots,
                       ncol = 1) +
  plot_annotation(title = "COMIT 2023 Single Station Glider Profiles",
  caption = "Calculated using Coppens <i>et al.</i> (1981)
               <br>
               <br>
               <img src='./www/cms_horiz.png' width='200'/>") &
  theme_osg()

plot(yoPlot)

###closest ctd-esque
profilePlot <- ggplot(data = yoDF %>%
         filter(!is.na(osg_soundvel1)),
                       aes(x=osg_soundvel1,
                           y=osg_i_depth,
                           color = gliderName,
                           linetype = cast
                       )) +
  geom_path(linewidth = 1
  ) +
  # geom_path(data = shipCast,
  #           aes(y = Depth..m.,
  #               x = SV..m.s.,
  #               color = "Hogarth",
  #               linetype = "Ship"),
  #           linewidth = 1) +
  scale_y_reverse() +
  scale_color_manual(values = c("blue", "gold", "black")) +
  #scale_colour_viridis_d(option = "cividis") +

  labs(#title = paste0("Single station profiles"),
    subtitle = "Single station profiles (at red line on left panel)",
       # caption = "Calculated using Coppens <i>et al.</i> (1981)
       #         <br>
       #         <br>
       #         <img src='./www/cms_horiz.png' width='200'/>",
       color = "Glider name",
       linetype = "Glider state",
       y = "Depth (m)",
       x = "Sound Speed (m/s)") +
  #facet_grid(~gliderName) +
  theme_osg() +
  theme(plot.title = element_text(vjust = 1.5))

plot(profilePlot)

ggsave(filename = "nearbyProfiles.png",
       plot = profilePlot,
       device = "png",
       path = "./COMIT",
       width = 16,
       height = 9)

#paneled plot
qlotz <- wrap_plots(plots) +
  plot_layout(guides = "collect",
              ncol = 1) &
  # plot_annotation(title = "COMIT 2023 Glider Sound Speed",
  #                 caption = "Calculated using Coppens <i>et al.</i> (1981)
  #              <br>
  #              <br>
  #              <img src='./www/cms_horiz.png' width='200'/>"
  # ) &
  theme_osg()
plot(qlotz)
nextPlot <- (qlotz | profilePlot) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "COMIT 2023 Glider Profiles",
                  caption = "Calculated using Coppens <i>et al.</i> (1981)
               <br>
               <br>
               <img src='./www/cms_horiz.png' width='200'/>") &
  theme_osg() &
  theme(plot.subtitle = element_text(vjust = 1.5))

plot(nextPlot)

# ggsave(filename = "paneledProfiles.png",
#        plot = nextPlot,
#        device = "png",
#        path = "./COMIT",
#        width = 16,
#        height = 9)

#### other sci data ####
# sciVars <- c("sci_water_cond", 
#              "sci_water_pressure", 
#              "sci_water_temp",
#          "sci_flbbcd_bb_units", 
#          "sci_flbbcd_cdom_units",
#          "sci_flbbcd_chlor_units",
#          "sci_oxy3835_oxygen",
#          "osg_salinity", 
#          "osg_soundvel1"
#          )

# sciplots <- list()
# for (g in unique(fulldf$gliderName)){
#   gf <- fulldf %>%
#     filter(gliderName == g)
#   
#   hf <- fulldf %>%
#     filter(m_water_depth > 0) %>%
#     filter(gliderName == g)
#   
#   for (v in unique(sciVars)) {
#     scf <- gf %>%
#       filter(.data[[v]] > 0) %>%
#       filter(!is.na(.data[[v]]))
#     
#     if (startsWith(v, "sci_water") | startsWith(v, "osg")){
#       scf <- scf %>%
#         filter(.data[[v]] > 3)
#     }
#     
#     sciplots[[g]][[v]] <- profilePlot(scf, m_present_time, osg_i_depth, .data[[v]]) +
#       theme_osg() +
#       theme(
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()
#       ) +
#       xlim(startSam, endSam) +
#       scale_color_viridis_c(
#         if (startsWith(v, "sci_water") | startsWith(v, "osg")){
#           limits =c(min(filter(fulldf, fulldf[[v]] > 3) %>%
#                           select(all_of(v)), na.rm = TRUE), max(fulldf[[v]], na.rm = TRUE))
#         } else {
#         limits =c(min(filter(fulldf, fulldf[[v]] > 0) %>%
#                                             select(all_of(v)), na.rm = TRUE), max(fulldf[[v]], na.rm = TRUE))
#         }
#       )
#   }
# }

flPlots <- list()
bbPlots <- list()
waterPlots <- list()
oxyPlots <- list()
svPlots <- list()
salPlots <- list()
cdomPlots <- list()

sci_water <- fulldf %>%
  group_by(gliderName) %>%
  filter(sci_water_temp > 10) #for bad rbr reads plus warm summer water

sci_flbb <- fulldf %>%
  group_by(gliderName) %>%
  filter(sci_flbbcd_bb_units > 0)

sci_oxy <- fulldf %>%
  group_by(gliderName) %>%
  filter(sci_oxy3835_oxygen > 0)

for (g in unique(fulldf$gliderName)){
  scidf <- sci_water %>%
    filter(gliderName == g)
  
waterPlots[[g]] <- profilePlot(scidf, m_present_time, osg_i_depth, sci_water_temp) +
  theme_osg() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  xlim(startSam, endSam) +
  # scale_color_viridis_c(limits = c(min(sci_water$sci_water_temp, na.rm = TRUE),
  #                                  max(sci_water$sci_water_temp, na.rm = TRUE))) +
  scale_color_cmocean(name = "thermal",
                      limits = c(min(sci_water$sci_water_temp, na.rm = TRUE),
                                 max(sci_water$sci_water_temp, na.rm = TRUE))) +
  labs(subtitle = g,
       y = "Depth (m)",
       x = "Date/Time (UTC)",
       color = "Water temperature (°C)")

salPlots[[g]] <- profilePlot(scidf, m_present_time, osg_i_depth, osg_salinity) +
  theme_osg() +
      labs(subtitle = g,
           y = "Depth (m)",
           x = "Date/Time (UTC)",
           color = "Salinity                  ") + #padding for legend alignment
  xlim(startSam, endSam) +
  scale_color_cmocean(name = "haline",
                      limits = c(min(sci_water$osg_salinity, na.rm = TRUE),
                                 max(sci_water$osg_salinity, na.rm = TRUE)))
  # scale_color_viridis_c(limits = c(min(sci_water$osg_salinity, na.rm = TRUE),
  #                                  max(sci_water$osg_salinity, na.rm = TRUE)))

svPlots[[g]] <- profilePlot(scidf, m_present_time, osg_i_depth, osg_soundvel1) +
  theme_osg() +
      labs(subtitle = g,
           y = "Depth (m)",
           x = "Date/Time (UTC)",
           color = "Sound speed (m/s)     ") +
  xlim(startSam, endSam) +
  scale_color_cmocean(name = "speed",
                      limits = c(min(sci_water$osg_soundvel1, na.rm = TRUE),
                                 max(sci_water$osg_soundvel1, na.rm = TRUE)))
  # scale_color_viridis_c(limits = c(min(sci_water$osg_soundvel1, na.rm = TRUE),
  #                                  max(sci_water$osg_soundvel1, na.rm = TRUE)))

scidf <- sci_flbb %>%
  filter(gliderName == g)

bbPlots[[g]] <- profilePlot(scidf, m_present_time, osg_i_depth, sci_flbbcd_bb_units) +
  theme_osg() +
      labs(subtitle = g,
           y = "Depth (m)",
           x = "Date/Time (UTC)") +
  xlim(startSam, endSam) +
  scale_color_cmocean(limits = c(min(sci_flbb$sci_flbbcd_bb_units, na.rm = TRUE),
                                   max(sci_flbb$sci_flbbcd_bb_units, na.rm = TRUE)),
                        name = "turbid")
  # scale_color_viridis_c(limits = c(min(sci_flbb$sci_flbbcd_bb_units, na.rm = TRUE),
  #                                  max(sci_flbb$sci_flbbcd_bb_units, na.rm = TRUE)),
  #                       option = "magma")

flPlots[[g]] <- profilePlot(scidf, m_present_time, osg_i_depth, sci_flbbcd_chlor_units) +
  theme_osg() +
      labs(subtitle = g,
           y = "Depth (m)",
           x = "Date/Time (UTC)") +
  xlim(startSam, endSam) +
  # scale_color_viridis_c(limits = c(min(sci_flbb$sci_flbbcd_chlor_units, na.rm = TRUE),
  #                                  max(sci_flbb$sci_flbbcd_chlor_units, na.rm = TRUE)),
  #                       option = "magma")
scale_color_cmocean(limits = c(min(sci_flbb$sci_flbbcd_chlor_units, na.rm = TRUE),
                                 max(sci_flbb$sci_flbbcd_chlor_units, na.rm = TRUE)),
                      name = "algae")

cdomPlots[[g]] <- profilePlot(scidf, m_present_time, osg_i_depth, sci_flbbcd_cdom_units) +
  theme_osg() +
      labs(subtitle = g,
           y = "Depth (m)",
           x = "Date/Time (UTC)") +
  xlim(startSam, endSam) +
  scale_color_cmocean(limits = c(min(sci_flbb$sci_flbbcd_cdom_units, na.rm = TRUE),
                                   max(sci_flbb$sci_flbbcd_cdom_units, na.rm = TRUE)),
                        name = "matter")
  # scale_color_viridis_c(limits = c(min(sci_flbb$sci_flbbcd_cdom_units, na.rm = TRUE),
  #                                  max(sci_flbb$sci_flbbcd_cdom_units, na.rm = TRUE)),
  #                       option = "magma")

scidf <- sci_oxy %>%
  filter(gliderName == g)

oxyPlots[[g]] <- profilePlot(scidf, m_present_time, osg_i_depth, sci_oxy3835_oxygen) +
  theme_osg() +
      labs(subtitle = g,
           y = "Depth (m)",
           x = "Date/Time (UTC)") +
  xlim(startSam, endSam) +
  scale_color_cmocean(limits = c(min(sci_oxy$sci_oxy3835_oxygen, na.rm = TRUE),
                                   max(sci_oxy$sci_oxy3835_oxygen, na.rm = TRUE)),
                        name = "oxy")
  # scale_color_viridis_c(limits = c(min(sci_oxy$sci_oxy3835_oxygen, na.rm = TRUE),
  #                                  max(sci_oxy$sci_oxy3835_oxygen, na.rm = TRUE)),
  #                       option = "inferno")

}
          
  flAll <- wrap_plots(flPlots, ncol = 2) +
    plot_layout(guides = "collect") &
    theme_osg() &
    theme(plot.subtitle = element_text(vjust = 1.5))
  bbAll <- wrap_plots(bbPlots, ncol = 2) +
    plot_layout(guides = "collect") +
    theme_osg() &
    theme(plot.subtitle = element_text(vjust = 1.5))
  cdomAll <- wrap_plots(cdomPlots, ncol = 2) +
    plot_layout(guides = "collect") +
    theme_osg() &
    theme(plot.subtitle = element_text(vjust = 1.5))
  
  allFL <- (flAll / cdomAll / bbAll) + 
    plot_annotation(title = "COMIT 2023 Glider Profiles",
                    caption = "<br>
               <br>
               <img src='./www/cms_horiz.png' width='200'/>") &
    theme_osg() &
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      plot.title = element_text(size = 32, hjust = 0)
    )
  
  plot(allFL)
  
  # ggsave(filename = "flbbcdProfiles.png",
  #        plot = allFL,
  #        device = "png",
  #        path = "./COMIT",
  #        width = 16,
  #        height = 9)
  
  oxyAll <- wrap_plots(oxyPlots, ncol = 2) + 
    plot_layout(guides = "collect") +
    plot_annotation(title = "COMIT 2023 Glider Profiles",
                    caption = "<br>
               <br>
               <img src='./www/cms_horiz.png' width='200'/>") &
    theme_osg()
  
  # ggsave(filename = "oxy3835Profiles.png",
  #        plot = oxyAll,
  #        device = "png",
  #        path = "./COMIT",
  #        width = 16,
  #        height = 9)
  
  waterAll <- wrap_plots(waterPlots, ncol = 2) +
    plot_layout(guides = "collect") +
    theme_osg() &
    theme(plot.subtitle = element_text(vjust = 1.5))
  svAll <- wrap_plots(svPlots, ncol = 2) +
    plot_layout(guides = "collect") +
    theme_osg() &
    theme(plot.subtitle = element_text(vjust = 1.5))
  salAll <- wrap_plots(salPlots, ncol = 2) +
    plot_layout(guides = "collect") +
    theme_osg() &
    theme(plot.subtitle = element_text(vjust = 1.5))
  
  physAll <- (waterAll / svAll / salAll) + 
    plot_layout(guides = "collect") +
    plot_annotation(title = "COMIT 2023 Glider Profiles",
                    caption = "Calculated using Coppens <i>et al.</i> (1981)
               <br>
               <br>
               <img src='./www/cms_horiz.png' width='200'/>") &
    theme_osg() &
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      plot.title = element_text(size = 32, hjust = 0)
    )
  
 plot(physAll)
 
 # ggsave(filename = "tempSVSalProfiles.png",
 #        plot = physAll,
 #        device = "png",
 #        path = "./COMIT",
 #        width = 16,
 #        height = 9)
 
 
#### map! ####
test <- yoDF %>%
  select(gliderName, i_lat, i_lon) %>%
  group_by(gliderName) %>%
  summarize(i_lat_m = mean(i_lat),
            i_lon_m = mean(i_lon))
  
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
   