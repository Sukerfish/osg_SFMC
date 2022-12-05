library(tidyverse)
library(hacksaw)
library(lubridate)

missionNumber <- "M120"

raw <- read.csv(paste0(missionNumber,".ssv"),
                sep="", #whitespace as delimiter
                x=as.numeric(x),
                header = T)[-1,] #remove first line from df (units)

#raw <- read_delim(paste0(missionNumber,".ssv"))

glider <- raw %>%
  #mutate(across(where(is.character), as.numeric(as.character(.))))
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

saveRDS(glider, file = (paste0(missionNumber,".rds")))