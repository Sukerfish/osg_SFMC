library(tidyverse) #formatting and syntax utilities
library(lubridate) #date handling

#load input glider dataset from Brewery
load("./thebrewery/Data/M149_usf-jaialai.RData")

flight_vars <- c(
  "m_present_time",
  "i_lat",
  "i_lon"
)

science_vars <- c(
  "sci_flbbcd_chlor_units",
  "sci_water_temp"
)

#take Brewery data and select desired columns
output <- gliderdf %>%
  select(any_of(flight_vars), any_of(science_vars)) %>% #pick desired columns
  filter(!is.na(sci_flbbcd_chlor_units)) #drop all rows with NA values in column

#save the file as csv
write.csv(output, "selected_data.csv", row.names = FALSE)