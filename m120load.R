library(tidyverse)
library(hacksaw)
library(lubridate)

headerraw <- read.csv("M120.ssv", sep=" ", nrows = 2, header = F)

nohead <- read.csv("M120.ssv", sep=" ", skip = 2, header = F)

headers <- headerraw %>%
  add_column(none = "none",
             .after = "V37") %>%
  mutate(V63 = "none") %>%
  select(c(V1:V63))

colnames(nohead) <- headers[1,]

glider <- nohead %>%
  select(!c(none)) %>%
  mutate(status = if_else(m_avg_depth_rate > 0, "dive", "climb")) %>%
  fill(status) %>%
  #convert from rad to degrees for some vars
  mutate(m_roll = m_roll * 180/pi) %>%
  mutate(m_heading = m_heading * 180/pi) %>%
  mutate(c_heading = c_heading * 180/pi) %>%
  mutate(m_fin = m_fin * 180/pi) %>%
  mutate(c_fin = c_fin * 180/pi) %>%
  mutate(m_pitch = m_pitch * 180/pi)

glider$m_present_time <- as_datetime(glider$m_present_time)

save(headers, glider, file = "M120.RData")