library(tidyverse) #formatting and syntax utilities
library(lubridate) #date handling

#load input glider dataset from Brewery
load("./thebrewery/Data/M149_usf-jaialai.RData")

#load MBARI NO3 data
nitrate_corrected <- read.csv("M149_SUNA_MBARI_NO3.csv") %>%
  mutate(m_present_time = as_datetime(DateTime)) #add column called m_present_time and format from DateTime

#take Brewery data and calculate average water depth before merge
output <- gliderdf %>%
  group_by(yo_id) %>%
  mutate(avg_water_depth = mean(m_water_depth, na.rm = TRUE)) %>%
  ungroup() %>% 
  left_join(nitrate_corrected, by = "m_present_time") %>% #join in MBARI data by m_present_time
  select(m_present_time, Pres, Temp, Sal, NSBE, N, i_lat, i_lon, avg_water_depth) %>% #pick desired columns
  filter(!is.na(N)) #drop all rows with NA values in column N

#save the file as csv
write.csv(output, "M149_SUNA_GPS_WDepths.csv", row.names = FALSE)
