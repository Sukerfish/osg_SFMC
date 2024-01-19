
library(tidyverse)
library(lubridate)
# library(leaflet)
# library(leaflet.extras2)
# library(htmlwidgets)
library(osgUtils)
# library(terra)
library(ggplot2)
library(cmocean)
library(viridis)
library(patchwork)

library(reshape2)
library(MBA)
library(mgcv)
library(marmap)
library(FNN)

missionList <- c(
  "M123_usf-jaialai",
  "M116_usf-jaialai",
  "M127_usf-jaialai"
) %>%
  sort()

depth <- c(30, 50, 100)

output <- list()
for (i in missionList){
  print(i) #watch progress
  holding <- new.env() #clear environment each time
  
load(paste0("./thebrewery/Data/", i, ".RData"), envir = holding) #load in

#process
depthDF <- holding$gliderdf %>%
  mutate(ddate = date(m_present_time)) %>%
  group_by(yo_id) %>%
  mutate(gDepth = max(osg_i_depth, na.rm = TRUE),
         wDepth = max(m_water_depth, na.rm = TRUE)) %>%
  #ungroup() %>%
  arrange(m_present_time, .by_group = FALSE)

yoDepth <- depthDF %>%
  distinct(yo_id, gDepth, wDepth) %>%
  filter(yo_id > 0)

deepestYo <- yoDepth$yo_id[which(abs(yoDepth$gDepth - max(yoDepth$gDepth, na.rm = TRUE)) == min(abs(yoDepth$gDepth - max(yoDepth$gDepth, na.rm = TRUE))))]

yos <- list()
for (j in seq_along(depth)){
  offshore <- yoDepth %>%
    filter(yo_id < deepestYo)
  
  yoyo <- offshore$yo_id[which(abs(offshore$wDepth - depth[j]) == min(abs(offshore$wDepth - depth[j])))]
  yos[[j]] <- depthDF %>%
    filter(yo_id == yoyo) %>%
    mutate(isobath = depth[j])
}

#export
output[[i]] <- bind_rows(yos)
}

isobathDF <- bind_rows(output, .id = "missionID") %>%
  separate(missionID,
           c("missionNum","gliderName"),
           sep = "_",
           remove = FALSE) %>%
  ungroup() %>%
  unite("isoMission",
        c(missionID, isobath),
        sep = "_",
        remove = FALSE) %>%
  mutate(isobath = as.factor(isobath))

# Manually extracted hexidecimal ODV colour palette
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")

varOI <- "sci_suna_nitrate_concentration"

# https://theoceancode.netlify.app/post/odv_figures/
odvPlot <- list()
for (d in levels(isobathDF$isobath)){
ctdDF <- isobathDF %>%
  mutate(ndate = as.numeric(ddate)) %>%
  mutate(!!varOI := ifelse(.data[[varOI]] < 0, 0, .data[[varOI]])) %>%
  #filter(varOI > 0) %>%
  filter(!is.na(.data[[varOI]])) %>%
  filter(isobath == d) %>%
  arrange(m_present_time)

ctd_mba <- mba.surf(ctdDF[c("ndate", "osg_i_depth", paste0(varOI))], no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('ndate', 'osg_i_depth'), value.name = paste0(varOI)) %>%
  mutate(ddate = as_date(ndate))

odvPlot[[d]] <- ggplot(data = ctd_mba, aes(x = ddate, y = osg_i_depth)) +
    geom_raster(aes(fill = .data[[varOI]])) +
    geom_contour(aes(z = .data[[varOI]]), binwidth = 2, colour = "black", alpha = 0.2) +
    geom_contour(aes(z = .data[[varOI]]), breaks = 20, colour = "black") +
    scale_fill_gradientn(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                         colours = rev(ODV_colours)) +
    labs(y = "depth (m)", x = "date", fill = "n", title = paste0(d, "-m isobath")) +
    scale_y_reverse() +
    coord_cartesian(expand = F)
}

(testPlot <- (wrap_plots(odvPlot, ncol = 3, guides = "collect")))

# ggsave(filename = "testODV.png",
#        plot = testPlot,
#        device = "png",
#        path = ".",
#        width = 16,
#        height = 9)


varOI <- "sci_suna_nitrate_concentration"
#varOI <- "sci_water_temp"

isoPlot <- list()
for (d in unique(isobathDF$isoMission)){
  tempSci <- isobathDF %>%
    filter(isoMission == d) %>%
    filter(!is.na(.data[[varOI]]))
  
  tempFli <- isobathDF %>%
    filter(isoMission == d)
  
  isoPlot[[d]] <- ggplot(data =
                           tempSci,
                     aes(x=m_present_time,
                         y=round(osg_i_depth, 2))) +
      geom_point(aes(color = .data[[varOI]]),
                 size = 2) +
      scale_y_reverse(limits = c(105, 0)) +
      geom_point(data = filter(tempFli, m_water_depth > 0),
                 aes(x = m_present_time,
                     y = m_water_depth),
                 color = "black",
                 size = 0.3,
                 na.rm = TRUE
      ) +
      scale_x_datetime(breaks = scales::breaks_width("day")) +
      theme_bw() +
      labs(title = paste0(d),
        y = "Depth (m)",
        x = paste0(tempFli$ddate[1])) +
    
    if (varOI == "sci_water_temp") {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "thermal") 
    } else if (varOI == "sci_water_pressure") {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "deep")
    } else if (varOI == "sci_water_cond") {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "haline")
    } else if (varOI == "sci_suna_nitrate_concentration") {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "tempo") 
    } else if (varOI == "sci_flbbcd_chlor_units" |
               varOI == "sci_bbfl2s_chlor_scaled" ) {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "algae") 
    } else if (varOI == "sci_flbbcd_cdom_units" |
               varOI == "sci_bbfl2s_cdom_scaled" ) {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "matter") 
    } else if (varOI == "sci_flbbcd_bb_units" |
               varOI == "sci_bbfl2s_bb_scaled" ) {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "turbid") 
    } else if (varOI == "sci_oxy3835_oxygen" |
               varOI == "sci_oxy4_oxygen" ) {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "oxy") 
    } else if (startsWith(varOI, "sci_ocr")) {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "solar") 
    } else if (varOI == "osg_soundvel1") {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "speed") 
    } else if (varOI == "osg_rho") {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "dense") 
    } else if (varOI == "osg_salinity") {
      scale_color_cmocean(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)),
                          name = "haline") 
    } else {
      scale_color_viridis_c(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE)))
    }
  
}
(testPlot <- (wrap_plots(isoPlot, ncol = 3, guides = "collect")))

# ggsave(filename = "testTS.png",
#        plot = testPlot,
#        device = "png",
#        path = ".",
#        width = 16,
#        height = 9)

isoPlotYo <- list()
for (d in unique(isobathDF$isoMission)){
  tempSci <- isobathDF %>%
    filter(isoMission == d) %>%
    filter(!is.na(.data[[varOI]])) %>%
    arrange(m_present_time)
  
  tempFli <- isobathDF %>%
    filter(isoMission == d) %>%
    arrange(m_present_time)
  
  isoPlotYo[[d]] <- ggplot(data =
                           tempSci,
                         aes(y=round(osg_i_depth, 2))) +
    geom_line(aes(x = .data[[varOI]])) +
    scale_y_reverse(limits = c(105, 0)) +
    scale_x_continuous(limits = c(min(isobathDF[[varOI]], na.rm = TRUE), max(isobathDF[[varOI]], na.rm = TRUE))) +
    #scale_x_datetime(breaks = scales::breaks_width("day")) +
    theme_bw() +
    labs(title = paste0(d),
         y = "Depth (m)",
         x = paste0(tempFli$ddate[1]))
  
}
(testPlot <- (wrap_plots(isoPlotYo, ncol = 3, guides = "collect")))

# ggsave(filename = "testTS.png",
#        plot = testPlot,
#        device = "png",
#        path = ".",
#        width = 16,
#        height = 9)


