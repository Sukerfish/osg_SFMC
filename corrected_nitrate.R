#library(osgUtils)
library(tidyverse)
library(lubridate)
#library(cmocean)
#library(ggplot2)
#library(patchwork)

load("./thebrewery/Data/M149_usf-jaialai.RData")

nitrate_corrected <- read.csv("M149_SUNA_MBARI_NO3.csv") %>%
  mutate(m_present_time = as_datetime(DateTime))

output <- gliderdf %>%
  group_by(yo_id) %>%
  mutate(avg_water_depth = mean(m_water_depth, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(nitrate_corrected) %>%
  select(m_present_time, Pres, Temp, Sal, NSBE, N, i_lat, i_lon, avg_water_depth) %>%
  filter(!is.na(N))

write.csv(output, "M149_SUNA_GPS_WDepths.csv", row.names = FALSE)
