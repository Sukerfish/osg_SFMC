library(tidyverse)
library(ggplot2)
library(lubridate)
library(patchwork)

load(paste0("./thebrewery/Data/M125_usf-stella.RData"))

plotethunk <- fullehunk %>%
    #filter(hour >= input$echohour2[1] & hour <= input$echohour2[2]) %>%
    group_by(segment) %>%
    mutate(seg_time = mean(m_present_time)) %>%
    ungroup() %>%
    mutate(seg_hour = hour(seg_time)) %>%
    mutate(cycle = case_when(seg_hour %in% c(11:23) ~ 'day',
                             seg_hour %in% c(1:10, 24) ~ 'night')) %>% # add day/night filter
    #filter(cycle %in% input$todTgram) %>%
    group_by(segment, r_depth, cycle) %>%
    mutate(avgDb = exp(mean(log(abs(value))))*-1) %>%
    #mutate(avgDbOLD = mean(value)) %>%
    ungroup()


whales <- read.csv("whale_gps.csv") %>%
  mutate(m_present_time = as_datetime(m_present_time))

whalesum <- whales %>%
  mutate(dayy = yday(m_present_time)) %>%
  group_by(dayy) %>%
  summarise(num = length(unique(det_number)))

zz <- plotethunk %>%
  mutate(dayy = yday(m_present_time)) %>%
  left_join(whalesum) 

#### pseudotimegram ####
  ggEchoTime <-
    ggplot(data = zz,
           aes(x = seg_time,
               y = r_depth,
               colour = avgDb,
           )) +
    geom_point(size = 4,
               pch = 15
    ) +
  # geom_vline(data = whales %>%
  #              filter(m_present_time %within% interval(min(zz$seg_time), max(zz$seg_time))),
  #            aes(xintercept = m_present_time,
  #                alpha = 0.7)) +
    #coord_equal() +
    #scale_color_viridis_c() +
    scale_y_reverse() +
    theme_bw() +
    labs(#title = paste0("Avg dB returns (per meter) at depth from ", input$echohistrange2[1], " to ", input$echohistrange2[2]),
         y = "Depth (m)",
         #x = "Date/Time (UTC)",
         x = "Date",
         colour = "average dB") +
    theme(#plot.title = element_text(size = 32),
          #axis.title = element_text(size = 16),
          #axis.text = element_text(size = 12),
          legend.key = element_blank()) +
    guides(size="none") +
      scale_colour_gradientn(colours = c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
                                         "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28"),
                             limits = c(-75, -30)
      )

ggEchoTime

whalez <- zz %>%
  select(seg_time, num) %>%
  #unique() %>% 
  replace(is.na(.), 0)

whaleLine <- ggplot(data = whalez,
                    aes(x = seg_time,
                        y = num)) +
  # geom_freqpoly(bins = length(unique(yday(as_datetime(gliderdf$m_present_time)))), inherit.aes = FALSE,
  #               aes(x = as_datetime(seg_time))) +
  geom_line() +
  theme_bw() +
  labs(#title = paste0("Avg dB returns (per meter) at depth from ", input$echohistrange2[1], " to ", input$echohistrange2[2]),
    y = "Number of detections per day",
    #x = "Date/Time (UTC)",
    x = "Date")

whaleLine

stuff <- ggEchoTime / whaleLine + plot_layout(#guides = "collect",
                                              axis_titles = "collect")
stuff
