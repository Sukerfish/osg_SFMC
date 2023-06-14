# source("./thebrewery/scripts/pseudogram.R")
# library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(egg)
library(scales)
# library(egg)
# library(scales)

gliderName <- "usf-stella"
gliderdfFull <- readRDS(paste0("/echos/", gliderName, "/M125.rds"))
load(paste0("/echos/", gliderName, "/glider_live.RData"))

head <- read.csv(paste0("/echos/", gliderName, "/oilvol.ssv"),
                 sep="", #whitespace as delimiter
                 skip=923,
                 nrows=1)

raw <- read.csv(paste0("/echos/", gliderName, "/oilvol.ssv"),
                sep="", #whitespace as delimiter
                skip=926,
                header = FALSE)

colnames(raw) <- colnames(head)

pumping <- raw %>%
  mutate(m_present_time = as_datetime(m_present_time)) #convert to POSIXct

pumping$m_present_time <- as_datetime(floor(seconds(pumping$m_present_time)))

ridge <- interval(ymd_hm("2023-04-08T16:30"), ymd_hm("2023-04-08T22:30"))

waterDepth <- gliderdfFull %>%
  select(m_present_time, m_water_depth) %>%
  filter(!is.nan(m_water_depth)) %>%
  filter(m_present_time %within% ridge) %>%
  filter(m_water_depth > 0)

ggplot(data = waterDepth,
       aes(x = m_present_time,
           y = m_water_depth)) +
  geom_point() +
  scale_y_reverse()

fart <- interval(ymd_hm("2023-03-11T04:00"), ymd_hm("2023-03-11T05:00"))

idk <- gliderdfFull %>%
  select(m_present_time, m_depth, m_roll, m_pitch) %>%
  filter(!is.nan(m_roll)) %>%
  filter(m_present_time %within% fart)

ggplot(data = idk,
       aes(x = m_present_time,
           y = m_pitch)) +
  geom_point() +
  geom_line() +
  scale_y_reverse()


###### pseudotimegram #####
pf <- fullehunk %>%
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

test <- interval(ymd_hm("2023-04-02T04:00"), ymd_hm("2023-04-03T05:00"))

qf <- pf %>%
  filter(m_present_time %within% test)

ggEchoTime <- 
  ggplot(data = qf,
         aes(x = m_present_time,
             y = r_depth,
             colour = value,
         )) +
  geom_point(size = 2,
             pch = 15
  ) +
  #coord_equal() +
  #scale_color_viridis_c() +
  scale_y_reverse() +
  theme_bw() +
  labs(#title = paste0("Avg dB returns (per meter) at depth from ", input$echohistrange2[1], " to ", input$echohistrange2[2]),
       y = "Depth (m)",
       #x = "Date/Time (UTC)",
       x = "Date",
       colour = "average dB") +
  theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.key = element_blank(),
        plot.caption = element_markdown()) +
  guides(size="none") +
    scale_colour_gradientn(colours = c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
                                       "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28"),
                           limits = c(-75, -30)
    )