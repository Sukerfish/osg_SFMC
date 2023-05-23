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
