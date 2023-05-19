###build pseudograms for whole mission

source("./thebrewery/scripts/pseudogram.R")
library(ggplot2)
library(tidyverse)
library(egg)
library(scales)

gliderName <- "usf-stella"
load(paste0("/echos/", gliderName, "/glider_live.RData"))


### pseudogram plotting ####
for (i in echoListraw$value){

  print(i)
  # process into long format for plotting
  ehunk <- pseudogram(paste0("/echos/layers/", i, ".ssv"),
                      paste0("/echos/depths/", i, ".ssv"))
  
  ggEcho <-
    ggplot(data = 
             ehunk,
           aes(x=m_present_time,
               y=p_depth,
               z=value)) +
    geom_point(
      aes(color = value),
      size = 6,
      pch = 15,
      na.rm = TRUE
    ) +
    scale_colour_gradientn(colours = c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
                                       "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28"),
                           limits = c(-75, -30)) +
    scale_y_reverse() +
    theme_bw() +
    labs(title = paste0(i, " Pseudogram"),
         y = "Depth (m)",
         x = "Date/Time (UTC)",
         colour = "dB") +
    theme(plot.title = element_text(size = 32),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.key = element_blank()) +
    guides(size="none") +
    scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))
  
  ggsave(filename = paste0(i, ".png"),
         plot = ggEcho,
         device = "png",
         path = "./pseudograms",
         width = 21,
         height = 9)
  
}

### ek runtimes ###
ekRunList <- list()
for (i in echoListraw$value){
  
  print(i)
  # process into long format for plotting
  ehunk <- pseudogram(paste0("/echos/layers/", i, ".ssv"),
                      paste0("/echos/depths/", i, ".ssv"))
  
  times <- ehunk %>%
    select(m_present_time) %>%
    distinct() %>%
    arrange()
  
  ekRunList[[i]]$start <- min(times$m_present_time)
  ekRunList[[i]]$end <- max(times$m_present_time)
  
}

ekRun <- bind_rows(ekRunList, .id = "segment")
  
write.csv(ekRun, file = "ekRunM125.csv")
