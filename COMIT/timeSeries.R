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

out <- fulldf %>%
  select(gliderName, segment, yo_id, cast, i_lat, i_lon, osg_i_depth, m_present_time) %>%
  filter(cast != "Surface") %>%
  filter(cast == "Downcast") %>%
  group_by(gliderName, yo_id) %>%
  summarise(profTime = mean(m_present_time, na.rm = TRUE),
            profLat = mean(i_lat, na.rm = TRUE),
            profLon = mean(i_lon, na.rm = TRUE),
            profDepth = max(osg_i_depth, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(gliderName, profTime, .keep_all = TRUE) %>%
  group_by(gliderName) %>%
  arrange(profTime, .by_group = TRUE)

#write.csv(out, file = "./COMIT/gliderProfileSummary.csv", row.names = FALSE)

svdf <- fulldf %>%
  filter(sci_water_cond > 3) %>%
  filter(sci_water_temp > 0)

minSV <- min(svdf$osg_soundvel1, na.rm = TRUE)
maxSV <- max(svdf$osg_soundvel1, na.rm = TRUE)

######## processing ########

yoDF <- svdf %>%
  group_by(gliderName, yo_id) %>%
  mutate(yoTime = mean(m_present_time, na.rm = TRUE),
         yoDepth = mean(osg_i_depth, na.rm = TRUE)) %>%
  filter(m_present_time %within% interval(startStella, endStella))

###closest ctd-esque
profilePlot <- ggplot(data = yoDF %>%
                        filter(!is.na(osg_soundvel1)),
                      aes(x=osg_soundvel1,
                          y=osg_i_depth,
                          color = m_present_time,
                          #linetype = cast
                      )) +
  geom_path(
  ) +
  # geom_path(data = shipCast,
  #           aes(y = Depth..m.,
  #               x = SV..m.s.,
  #               color = "Hogarth",
  #               linetype = "Ship"),
  #           linewidth = 1) +
  scale_y_reverse() +
  #scale_color_manual(values = c("blue", "gold", "black")) +
  scale_colour_viridis_c(option = "cividis",
                         labels=as_datetime) +
  
  labs(title = paste0("Glider profiles over time"),
       caption = "Calculated using Coppens <i>et al.</i> (1981)
               <br>
               <br>
               <img src='./www/cms_horiz.png' width='200'/>",
       color = "Time (UTC)",
       linetype = "Glider state",
       y = "Depth (m)",
       x = "Sound Speed (m/s)") +
  #facet_grid(~gliderName) +
  theme_osg() +
  theme(plot.title = element_text(vjust = 1.5),
        strip.text.x = element_text(size = 16),
        strip.background.x = element_blank()) +
  facet_grid(~gliderName)

plot(profilePlot)

# ggsave(filename = "svOverTime.png",
#        plot = profilePlot,
#        device = "png",
#        path = "./COMIT",
#        width = 16,
#        height = 9)

#### depth based profiles ####
d = 56

depthDF <- fulldf %>%
  group_by(gliderName, yo_id) %>%
  filter(m_present_time %within% interval(startStella, endStella)) %>%
  mutate(yoTime = mean(m_present_time, na.rm = TRUE),
         yoDepth = mean(osg_i_depth, na.rm = TRUE),
         yoMax = max(m_water_depth, na.rm = TRUE)) %>%
  filter(sci_water_cond > 3) %>%
  filter(sci_water_temp > 0) %>%
  filter(gliderName == "usf-stella") %>%
  mutate(grp = ifelse(yoMax < d, "shallow", "deep"))
  #filter(yoMax < 53)

###closest ctd-esque
depthPlot <- ggplot(data = depthDF %>%
                        filter(!is.na(osg_soundvel1)),
                      aes(x=osg_soundvel1,
                          y=osg_i_depth,
                          color = m_present_time,
                          #linetype = cast
                      )) +
  geom_path(
  ) +
  # geom_path(data = shipCast,
  #           aes(y = Depth..m.,
  #               x = SV..m.s.,
  #               color = "Hogarth",
  #               linetype = "Ship"),
  #           linewidth = 1) +
  scale_y_reverse() +
  #scale_color_manual(values = c("blue", "gold", "black")) +
  scale_colour_viridis_c(option = "cividis",
                         labels=as_datetime) +
  
  labs(title = paste0("Glider profiles by depth usf-stella"),
       caption = "Calculated using Coppens <i>et al.</i> (1981)
               <br>
               <br>
               <img src='./www/cms_horiz.png' width='200'/>",
       color = "Time (UTC)",
       linetype = "Glider state",
       y = "Depth (m)",
       x = "Sound Speed (m/s)") +
  #facet_grid(~gliderName) +
  theme_osg() +
  theme(plot.title = element_text(vjust = 1.5),
        strip.text.x = element_text(size = 16),
        strip.background.x = element_blank()) +
  facet_grid(~grp,
             labeller = labeller(grp = c(`deep` = paste0("Deep (", "\u2265", d, "m water depth)"), 
                                         `shallow` = paste0("Shallow (<", d, "m water depth)"))))

plot(depthPlot)

# ggsave(filename = "svByDepth.png",
#        plot = depthPlot,
#        device = "png",
#        path = "./COMIT",
#        width = 16,
#        height = 9)

#######
###closest ctd-esque
yoanim <- fulldf %>%
  group_by(gliderName, yo_id) %>%
  filter(m_present_time %within% interval(startStella, endStella)) %>%
  mutate(yoTime = mean(m_present_time, na.rm = TRUE),
         yoDepth = mean(osg_i_depth, na.rm = TRUE),
         yoMax = max(m_water_depth, na.rm = TRUE),
         yoFloorTime = floor_date(yoTime, "1 minute")) %>%
  ungroup() %>%
  #group_by(yoFloorTime) %>%
  #filter(n_distinct(gliderName) == n_distinct(fulldf$gliderName)) %>%
  filter(!is.na(osg_soundvel1)) %>%
  filter(sci_water_cond > 3) %>%
  filter(sci_water_temp > 0) %>%
  filter(cast != "Surface") %>%
  filter(gliderName == "usf-stella")

animPlot <- ggplot(data = yoanim,
                    aes(x=osg_soundvel1,
                        y=osg_i_depth,
                        #color = m_present_time,
                        linetype = cast
                    )) +
  geom_path(
  ) +
  geom_hline(aes(yintercept = yoMax)) +
  # geom_path(data = shipCast,
  #           aes(y = Depth..m.,
  #               x = SV..m.s.,
  #               color = "Hogarth",
  #               linetype = "Ship"),
  #           linewidth = 1) +
  scale_y_reverse() +
  #scale_color_manual(values = c("blue", "gold", "black")) +
  scale_colour_viridis_c(option = "cividis",
                         labels=as_datetime) +
  
  # labs(title = paste0("Glider profiles over time"),
  #      caption = "Calculated using Coppens <i>et al.</i> (1981)
  #              <br>
  #              <br>
  #              <img src='./www/cms_horiz.png' width='200'/>",
  #      color = "Time (UTC)",
  #      linetype = "Glider state",
  #      y = "Depth (m)",
  #      x = "Sound Speed (m/s)") +
  #facet_grid(~gliderName) +
  theme_osg() +
  theme(plot.title = element_text(vjust = 1.5),
        plot.subtitle = element_text(vjust = 1.5),
        strip.text.x = element_text(size = 16),
        strip.background.x = element_blank()) +
  #facet_grid(~gliderName) +
  ##gganimate stuff
  transition_states(yoFloorTime,
                    transition_length = 2,
                    state_length = 1) +
  #transition_time(hour) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade() +
  labs(
    title = "Profiles through time usf-stella",
    subtitle = 'Time of profile: {closest_state} UTC',
    #subtitle = 'Frame {frame} of {nframes}'
    caption = "Calculated using Coppens <i>et al.</i> (1981)
                 <br>
                 Black horizontal line is greatest water depth measured by glider altimeter
                 <br>
                 <br>
                 <img src='./www/cms_horiz.png' width='200'/>",
    #color = "Time (UTC)",
    linetype = "Glider state",
    y = "Depth (m)",
    x = "Sound Speed (m/s)"
  )
 
animate(animPlot, height = 800, width = 800, end_pause = 30)
# anim_save(filename = "stellaProfiles.gif",
#           animation = animPlot,
#           #device = "png",
#           path = "./COMIT",
#           width = 800,
#           height = 800, 
#           end_pause = 30)

########## depth colormaps #######
colorAnim <- fulldf %>%
  group_by(gliderName, yo_id) %>%
  filter(m_present_time %within% interval(startStella, endStella)) %>%
  mutate(yoTime = mean(m_present_time, na.rm = TRUE),
         yoDepth = mean(osg_i_depth, na.rm = TRUE),
         yoMax = max(m_water_depth, na.rm = TRUE),
         yoFloorTime = floor_date(yoTime, "3 hour")) %>%
  ungroup() %>%
  #group_by(yoFloorTime) %>%
  #filter(n_distinct(gliderName) == n_distinct(fulldf$gliderName)) %>%
  filter(!is.na(osg_soundvel1)) %>%
  filter(sci_water_cond > 3) %>%
  filter(sci_water_temp > 0) %>%
  filter(cast != "Surface") %>%
  filter(gliderName == "usf-sam")

colorPlot <- ggplot(data = colorAnim,
       aes(x=m_present_time,
           y=osg_i_depth,
           #z=osg_soundvel1
       )) +
  geom_point(
    aes(color = osg_soundvel1)
  ) +
  #geom_hline(yintercept = 0) +
  #xlim(startSam, endSam) +
  scale_y_reverse() +
  scale_colour_viridis_c(limits = c(minSV, maxSV)) +
  geom_point(#data = wf,
             aes(y = m_water_depth),
             size = 0.3,
             color = "black",
             na.rm = TRUE
  ) +
  theme_bw() +
  labs(#subtitle = i,
       # caption = "Calculated using Coppens <i>et al.</i> (1981) 
       #         <br>
       #         <br>
       #         <img src='./www/cms_horiz.png' width='200'/>",
       color = "Sound speed (m/s)",
       y = "Depth (m)",
       x = "Date/Time (UTC)") +
  transition_states(yoFloorTime,
                    transition_length = 2,
                    state_length = 1) +
  #transition_time(hour) +
  ease_aes('linear')
# theme_osg() + 
# geom_vline(xintercept = as_datetime(max(outs$time_point)),
#            linewidth = 1)

animate(colorPlot, height = 800, width = 800)

#### depth based profiles ####
mixDF <- fulldf %>%
  group_by(gliderName, yo_id) %>%
  #filter(m_present_time %within% interval(startStella, endStella)) %>%
  mutate(yoTime = mean(m_present_time, na.rm = TRUE),
         yoDepth = mean(osg_i_depth, na.rm = TRUE),
         yoMax = max(m_water_depth, na.rm = TRUE),
         yoFloorTime = floor_date(yoTime, "3 hour")) %>%
  ungroup() %>%
  group_by(yo_id) %>%
  #group_by(yoFloorTime) %>%
  #filter(n_distinct(gliderName) == n_distinct(fulldf$gliderName)) %>%
  filter(!is.na(sci_flbbcd_chlor_units)) %>%
  #filter(sci_water_cond > 3) %>%
 # filter(sci_water_temp > 0) %>%
  filter(cast != "Surface") %>%
  filter(gliderName == "usf-sam")

###closest ctd-esque
mixPlot <- ggplot(data = mixDF,
                    aes(x=m_present_time,
                        y=osg_i_depth,
                        #z=osg_soundvel1
                    )) +
  geom_point(
    aes(color = sci_flbbcd_chlor_units)
  ) +
  #geom_hline(yintercept = 0) +
  #xlim(startSam, endSam) +
  scale_y_reverse() +
  scale_colour_viridis_c() +
  # geom_point(#data = wf,
  #   aes(y = m_water_depth),
  #   size = 0.3,
  #   color = "black",
  #   na.rm = TRUE
  # ) +
  theme_bw() +
  labs(#subtitle = i,
    # caption = "Calculated using Coppens <i>et al.</i> (1981) 
    #         <br>
    #         <br>
    #         <img src='./www/cms_horiz.png' width='200'/>",
    color = "Chlor",
    y = "Depth (m)",
    x = "Date/Time (UTC)")
  #facet_grid(~gliderName)

plot(mixPlot)
plot_gg(mixPlot, multicore=TRUE, height=5, width=6, scale=500, windowsize = c(1200, 960))
render_camera(fov = 50, zoom = 0.6, theta = 45, phi = 25)
render_snapshot()