### sound velocity publishing ###

library(tidyverse)

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
             cast, yo_id)) #select only vars of interest
  
  out <- list() #initialize list for segment by segment calcs
  for (j in unique(svpdf$segment)) {
    segment <- j %>%
      str_remove(pattern = ".ssv")
    
    out[[segment]] <- svpdf %>%
      filter(segment == j) %>%
      filter(yo_id > 0) %>%
      filter(cast == "Downcast" | cast == "Upcast") %>%
      group_by(yo_id, cast) %>%
      mutate(p_lat = mean(i_lat, na.rm = TRUE),
             p_lon = mean(i_lon, na.rm = TRUE),
             p_time = mean(m_present_time, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(yo_id) %>%
      mutate(y_lat = mean(i_lat, na.rm = TRUE),
             y_lon = mean(i_lon, na.rm = TRUE),
             y_time = mean(m_present_time, na.rm = TRUE)) %>%
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
          select(c(osg_i_depth, osg_soundvel1)) %>%
          filter(!is.na(osg_soundvel1)) #select vars of interest and remove NAs
        
        #write csv for each yo within cast within segment
        write.csv(yop,
                  file = paste0("/echos/gvp/", i, "/", l, "/", k, "_yo_", y, ".csv"))
      }
        
    }
  }
  
  
}
  

  plot <- 
    ggplot(
      data = out[[segment]],
      aes(x = osg_soundvel1,
          y = osg_i_depth,
          color = cast,
          #shape = variable
      )) +
    geom_point_interactive(size = 3,
                           pch = 1) +
    # scale_color_gradient(
    #   low = "red",
    #   high = "blue",
    # ) +
    #coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
    scale_y_reverse() +
    theme_bw() +
    labs(title = paste0(segment, " SVP"),
         x = "Sound Velocity (m/s)",
         y = "Depth (m)",
         #color = "Time",
         #caption = "Red = older ... Blue = more recent",
         #caption = "<img src='./www/cms_horiz.png' width='200'/>"
    ) +
    theme(plot.title = element_text(size = 32),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          plot.caption = element_markdown(),
          #legend.position ="none",
    ) 
