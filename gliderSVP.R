### sound velocity publishing ###

library(tidyverse)
library(lubridate)
library(stringr)

my.write <- function(x, file, header, f = write.csv, ...){
  # create and open the file connection
  datafile <- file(file, open = 'wt')
  # close on exit
  on.exit(close(datafile))
  # if a header is defined, write it to the file (@CarlWitthoft's suggestion)
  if(!missing(header)) writeLines(header,con=datafile)
  # write the file using the defined function and required addition arguments  
  f(x, datafile,...)
}

#get deployed gliders
deployedGliders <- read.csv("/echos/deployedGliders.txt", 
                            sep = "",
                            header = FALSE)
colnames(deployedGliders)[1] = "Name"
colnames(deployedGliders)[2] = "ahrCap"

#only process "real" ones
deployedGliders <- deployedGliders %>%
  filter(!str_starts(Name,"#")) #remove any commented lines

#initialize list
gliders_live <- list()
for (i in deployedGliders$Name){
  
  #load latest live data file
  load(paste0("/echos/", i, "/glider_live.RData"))
  
  svpdf <- data.frame() #initialize new sv dataframe
  svpdf <- gliderdf %>%
    select(c(m_present_time, segment, i_lat, i_lon, 
             osg_i_depth, osg_soundvel1,
             cast, yo_id, m_water_depth, sci_water_temp, osg_salinity)) #select only vars of interest
  
  out <- list() #initialize list for segment by segment calcs
  for (j in unique(svpdf$segment)) {
    segment <- j %>%
      str_remove(pattern = ".ssv") #get segment identifier
    
    out[[segment]] <- svpdf %>%
      filter(segment == j) %>%
      filter(yo_id > 0) %>% #check if yo was ID'd
      filter(cast == "Downcast" | cast == "Upcast") %>% #ensure no surface/unknown intervals
      group_by(yo_id, cast) %>% #calculate averages per profile
      mutate(p_lat = mean(i_lat, na.rm = TRUE),
             p_lon = mean(i_lon, na.rm = TRUE),
             p_time = mean(m_present_time, na.rm = TRUE),
             p_min_depth = min(osg_i_depth, na.rm = TRUE),
             p_max_depth = max(osg_i_depth, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(yo_id) %>% #calculate averages per yo
      mutate(y_lat = mean(i_lat, na.rm = TRUE),
             y_lon = mean(i_lon, na.rm = TRUE),
             y_time = mean(m_present_time, na.rm = TRUE),
             #y_water_depth = mean(filter(., m_water_depth > 0), na.rm = TRUE),
             y_min_depth = min(osg_i_depth, na.rm = TRUE),
             y_max_depth = max(osg_i_depth, na.rm = TRUE)) %>%
      ungroup()
    
  }
  
  segprof <- data.frame() #initialize
  segprof <- bind_rows(out, .id = "segment")
  
  for (k in unique(segprof$segment)) { #first layer, go by segment
    svp <- data.frame()
    svp <- segprof %>%
      filter(segment == k)
    for (l in unique(svp$cast)) { #then by upcast/downcast
      castp <- data.frame()
      castp <- svp %>%
        filter(cast == l)
      for (y in unique(castp$yo_id)) { #then by yo within
        yop <- data.frame()
        yop <- castp %>%
          filter(yo_id == y) %>%
          filter(!is.na(osg_soundvel1))  #remove NAs
          #arrange(osg_i_depth) #save as top to bottom?
        
        yodat <- head(yop, 1)
        
        #format metadata for header in .asvp format
        asvp <- paste0("( SoundVelocity 1.00 ", y, " ", strftime(Sys.time(), format = "%Y%m%d%H%M"), " ", #file ID/creation time
                           round(yodat$p_lat, 7), " ", round(yodat$p_lon, 7), " ", #profile lat/lon
                           "4500", " ", #radius of validity
                           strftime(min(yop$m_present_time), format = "%Y%m%d%H%M", tz = "UTC"), #valid start time, force m_present_time as UTC
                           " ", strftime(max(yop$m_present_time), format = "%Y%m%d%H%M", tz = "UTC"), " ", #valid end time
                           i, " PE ", nrow(yop), " )") #platform name, history of modifications, number of samples
        
        #write csv for each yo within cast within segment
        my.write(yop %>%
                   select(c(osg_i_depth, osg_soundvel1)) %>%
                   round(., 4), 
                 paste0("/echos/gvp/", i, "/", l, "/", k, "_yo_", y, ".asvp"), header = asvp, 
                 f = write.table, row.names = FALSE, col.names = FALSE)
        
        #format metadata for header in .edf format
          write(c(paste0("Date of Launch: ", strftime(yodat$p_time, format = "%m/%d/%Y", tz = "UTC")),
                  paste0("Time of Launch: ", strftime(yodat$p_time, format = "%H:%M:%S", tz = "UTC")),
                  paste0("Sequence # : ", y),
                  paste0("Latitude : ", round(yodat$p_lat, 7)),
                  paste0("Longitude : ", round(yodat$p_lon, 7)),
                  paste0("Display Units : Metric"),
                  paste0("Depth (m) - Temperature (C) - Sound Velocity (m/s) - Salinity (ppt)")), 
                file = paste0("/echos/gvp/", i, "/", l, "/", k, "_yo_", y, ".edf"))
        
        #write edf for each yo within cast within segment
        write.table(yop %>%
                   select(c(osg_i_depth, sci_water_temp, osg_soundvel1, osg_salinity)) %>%
                   round(., 4), 
                   file = paste0("/echos/gvp/", i, "/", l, "/", k, "_yo_", y, ".edf"), 
                   row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
      }
        
    }
  }
  
  
}
  

  # plot <- 
  #   ggplot(
  #     data = out[[segment]],
  #     aes(x = osg_soundvel1,
  #         y = osg_i_depth,
  #         color = cast,
  #         #shape = variable
  #     )) +
  #   geom_point_interactive(size = 3,
  #                          pch = 1) +
  #   # scale_color_gradient(
  #   #   low = "red",
  #   #   high = "blue",
  #   # ) +
  #   #coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
  #   scale_y_reverse() +
  #   theme_bw() +
  #   labs(title = paste0(segment, " SVP"),
  #        x = "Sound Velocity (m/s)",
  #        y = "Depth (m)",
  #        #color = "Time",
  #        #caption = "Red = older ... Blue = more recent",
  #        #caption = "<img src='./www/cms_horiz.png' width='200'/>"
  #   ) +
  #   theme(plot.title = element_text(size = 32),
  #         axis.title = element_text(size = 16),
  #         axis.text = element_text(size = 12),
  #         plot.caption = element_markdown(),
  #         #legend.position ="none",
  #   ) 
