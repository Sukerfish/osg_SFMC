library(tidyverse)
library(ggplot2)
library(shiny)
library(patchwork)
library(cowplot)
library(plotly)
library(oce)
library(ocedata)
library(PlotSvalbard) #devtools::install_github("MikkoVihtakari/PlotSvalbard", upgrade = "never")
library(gsw)

#library(Cairo)   # For nicer ggplot2 output when deployed on Linux

#library(lubridate)

# missionNumber <- "output"
# 
# head <- read.csv(paste0(missionNumber,".ssv"),
#                  sep="", #whitespace as delimiter
#                  nrows=1,
#                  skip=500)
# 
# raw <- read.csv(paste0(missionNumber,".ssv"),
#                 sep="", #whitespace as delimiter
#                 skip=503,
#                 header = FALSE)
# 
# colnames(raw) <- colnames(head)
# 
# raw <- raw %>%
#   mutate(m_present_time = as_datetime(m_present_time)) #convert to POSIXct
# 
# raw$m_present_time <- as_datetime(floor(seconds(raw$m_present_time)))

# fileList <- list.files(path = ".",
#            pattern = "*.RData")

glider <- read_rds("M118.rds")
# glider$m_present_time <- as_datetime(floor(seconds(glider$m_present_time)))
# 
# glider <- glider %>%
#   left_join(raw)

#https://rdrr.io/github/AustralianAntarcticDivision/ZooScatR/src/R/soundvelocity.R
c_Coppens1981 <- function(D,S,T){
  t <- T/10
  D = D/1000
  c0 <- 1449.05 + 45.7*t - 5.21*(t^2)  + 0.23*(t^3)  + (1.333 - 0.126*t + 0.009*(t^2)) * (S - 35)
  c <- c0 + (16.23 + 0.253*t)*D + (0.213-0.1*t)*(D^2)  + (0.016 + 0.0002*(S-35))*(S- 35)*t*D
  return(c)
}

#pull out science variables
scivars <- glider %>%
  select(starts_with("sci")) %>%
  colnames()

#pull out flight variables
flightvars <- glider %>%
  select(!starts_with("sci")) %>%
  colnames()

input <- glider %>%
  select(c(m_roll, m_pitch)) %>%
  colnames()

chunk <- glider %>%
  filter(m_present_time >= "2022-08-15" & m_present_time < "2022-08-19") %>%
  mutate(status = if_else(m_avg_depth_rate > 0, "dive", "climb")) %>%
  fill(status) %>%
  #filter(status == "dive") %>%
  mutate(soundvel1 = c_Coppens1981(sci_rbrctd_depth_00,
                                   sci_rbrctd_salinity_00,
                                   sci_rbrctd_temperature_00))
  #mutate(conTemp = gsw_CT_from_t(sci_rbrctd_salinity_00, sci_rbrctd_temperature_00, sci_rbrctd_pressure_00)) %>%
  #mutate(soundvel2 = gsw_sound_speed(sci_rbrctd_salinity_00, conTemp, sci_rbrctd_pressure_00))
  
# pings <- chunk %>%
#   filter(!is.na(m_water_depth)) %>%
#   mutate(pingTime = 2*sci_rbrctd_depth_00*1540) #D = 1/2*v*t

chunkSummary <- chunk %>%
  select(all_of(input)) %>%
  summarise(across(everything(), list(stdev = ~ sd(.x, na.rm = TRUE), mean = ~ mean(.x, na.rm = TRUE))))
            
zunk <- chunk %>%
  select(all_of(input)) %>%
  mutate(across(m_roll, zscore = ((avg - LTmean)/stdev)))
  
#glider$m_present_time <- as_datetime(floor(seconds(glider$m_present_time)))
yunk <- glider %>%
  select(!c(status))

#ballast pump delta method ... and max battpos for surface
qunk <- glider %>%
  select(m_present_time, c_ballast_pumped, c_battpos) %>%
  filter(!is.nan(c_ballast_pumped)) %>%
  filter(!is.nan(c_battpos)) %>%
  mutate(batt_max = ifelse(c_battpos == max(c_battpos, na.rm = TRUE), 1, 0)) %>%
  mutate(pump_delta = c_ballast_pumped - lead(c_ballast_pumped)) %>%
  mutate(batt_delta = c_battpos - lead(c_battpos)) %>%
  #mutate(pump_max = ifelse(c_ballast_pumped == max(c_ballast_pumped, na.rm = TRUE), 1, 0)) %>%
  mutate(status = ifelse(batt_delta == 0 & batt_max == 1, "surface",
                         ifelse(pump_delta >= 100, "dive", 
                                ifelse(pump_delta <= -100, "climb", NA)))) %>%
  #mutate(status = ifelse(batt_delta == 0 & batt_max == 1, "surface", NA)) %>%
  fill(status) %>%
  select(m_present_time, status) %>%
  full_join(yunk) %>%
  arrange(m_present_time) %>%
  fill(status)
  

# ts_plot(chunk %>%
#           filter(!is.na(sci_water_temp)),
#         temp_col = "sci_water_temp",
#         sal_col = "sci_rbrctd_salinity_00",
#         #xlim = c(min(chunk$sci_rbrctd_salinity_00, na.rm = TRUE), max(chunk$sci_rbrctd_temperature_00, na.rm = TRUE)),
#         #ylim = c(min(chunk$sci_rbrctd_temperature_00, na.rm = TRUE), max(chunk$sci_rbrctd_temperature_00, na.rm = TRUE)),
#         zoom = TRUE,
# )

plotup <- list()
for (i in input){
  plotup[[i]] = ggplot(data = select(chunk, m_present_time, all_of(i)) %>%
                         pivot_longer(
                           cols = !m_present_time,
                           names_to = "variable",
                           values_to = "count") %>%
                         filter(!is.na(count)),
                       aes(x = m_present_time,
                           y = count,
                           color = variable,
                           shape = variable)) +
    geom_point() +
    ylab(i) +
    #coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
    theme_minimal()
}

wrap_plots(plotup, ncol = 1)
colorvec <- c("red","blue","purple","yellow","green")
aligned_plots <- align_plots(plotup[[1]], plotup[[2]], align="hv", axis="tblr")
ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])

wrap_elements(get_plot_component(plotup[[1]], "ylab-l")) +
  wrap_elements(get_y_axis(plotup[[1]])) +
  wrap_elements(get_plot_component(plotup[[2]], "ylab-1")) +
  wrap_elements(get_y_axis(plotup[[2]])) +
  wrap_l
  plot_layout(widths = c(3, 1, 3, 1, 40))



ggplot(
  data =
    select(chunk, m_present_time, all_of(input)) %>%
    pivot_longer(
      cols = !m_present_time,
      names_to = "variable",
      values_to = "count"
    ) %>%
    filter(!is.na(count)),
  aes(x = m_present_time,
      y = count,
      color = variable)
) +
  geom_point()
