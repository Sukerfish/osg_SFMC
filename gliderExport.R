### ssv publishing ###

library(tidyverse)
library(stringr)

#get deployed gliders
deployedGliders <- read.csv("/echos/deployedGliders.txt", 
                            sep = "",
                            header = FALSE)
colnames(deployedGliders)[1] = "Name"
colnames(deployedGliders)[2] = "ahrCap"

#only process "real" ones
deployedGliders <- deployedGliders %>%
  filter(!str_starts(Name,"#")) #remove any commented lines

needVars <- c("m_present_time", "m_depth", "m_altitude", "m_water_depth", "m_pitch", "m_roll",
              "m_gps_lat", "m_gps_lon", "m_lat", "m_lon", "sci_water_pressure", "m_speed",
              "sci_water_cond", "sci_water_temp", "sci_bbfl2s_chlor_scaled", "sci_flbbcd_chlor_units",
              "sci_oxy3835_oxygen", "sci_oxy4_oxygen", "sci_rbrctd_salinity_00")

#initialize list
gliders_live <- list()
for (i in deployedGliders$Name){
  
  #load latest live data file
  load(paste0("/echos/", i, "/glider_live.RData"))
  
  for (j in unique(gliderdf$segment)) {
    seg <- j %>%
      str_remove(pattern = ".ssv") #get segment identifier
    
    segdf <- gliderdf %>%
      filter(segment == j) %>%
      select(any_of(needVars)) %>%
      mutate(m_present_time = as.numeric(m_present_time))
    
    write.table(segdf, 
                file = paste0("/echos/segments/", i, "/", seg, ".ssv"), 
                row.names = FALSE, col.names = TRUE)
    
  }
}