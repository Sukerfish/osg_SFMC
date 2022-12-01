library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
#library(ZooScatR)

load("M120.RData")

# vars <- 
#   c("sci_rbrctd_salinity_00", "m_water_depth", "m_present_time")

#out$m_present_time <- as_datetime(out$m_present_time)

#https://rdrr.io/github/AustralianAntarcticDivision/ZooScatR/src/R/soundvelocity.R
c_Coppens1981 <- function(D,S,T){
  t <- T/10
  D = D/1000
  c0 <- 1449.05 + 45.7*t - 5.21*(t^2)  + 0.23*(t^3)  + (1.333 - 0.126*t + 0.009*(t^2)) * (S - 35)
  c <- c0 + (16.23 + 0.253*t)*D + (0.213-0.1*t)*(D^2)  + (0.016 + 0.0002*(S-35))*(S- 35)*t*D
  return(c)
}

c_Leroy08 <- function(Z,T,S,lat){
  c <- 1402.5 + 5*T - 5.44 * 10^-2*T^2 + 2.1 * 10^-4*T^3 +
    1.33*S - 1.23 * (10^-2)*S*T+8.7*(10^-5)*S*T^2 +
    1.56*(10^-2)*Z+2.55*(10^-7)*Z^2-7.3*(10^-12)*Z^3+
    1.2*(10^-6)*Z*(lat-45)-9.5*(10^-13)*T*Z^3+
    3*(10^-7)*T^2*Z+1.43*(10^-5)*S*Z
  return(c)
}

chunk <- glider %>%
  #filter(m_present_time >= "2022-10-19" & m_present_time < "2022-10-21") %>%
  # mutate(status = if_else(m_avg_depth_rate > 0, "dive", "climb")) %>%
  # fill(status) %>%
  #filter(status == "dive")
  mutate(soundvel1 = c_Coppens1981(sci_rbrctd_depth_00,
                       sci_rbrctd_salinity_00,
                       sci_rbrctd_temperature_00)) %>%
  mutate(soundvel2 = c_Leroy08(sci_rbrctd_depth_00,
                               sci_rbrctd_temperature_00,
                               sci_rbrctd_salinity_00,
                               m_gps_lat
                               ))

test <- chunk %>%
  filter(!(is.na(soundvel2)
           | is.na(m_depth)))

test <- chunk %>%
  filter(!(is.na(sci_rbrctd_salinity_00) | is.na(sci_rbrctd_temperature_00)
         | is.na(sci_rbrctd_depth_00)))

p1 <- ggplot(data=test,
       aes(x=m_present_time,
           y=sci_rbrctd_depth_00,
           z=soundvel1)) +
  geom_point(
    aes(color = soundvel1)
  ) +
  scale_y_reverse() +
  scale_colour_viridis_c(limits = c(1515,
                                    max(chunk$soundvel1, na.rm = TRUE)))

mean(chunk$soundvel, na.rm = TRUE)-5*sd(chunk$soundvel, na.rm = TRUE)

  scale_colour_gradientn(colours = heat.colors(10),
                         limits = c(min(test$sci_rbrctd_conductivity_00, na.rm = TRUE)
                         ,max(test$sci_rbrctd_conductivity_00, na.rm = TRUE)))
  
# p2 <- ggplot(data=test,
#              aes(x=m_present_time,
#                  y=m_depth,
#                  z=sci_water_temp)) +
#   geom_point(
#     aes(color = sci_water_temp)
#   ) +
#   scale_y_reverse() +
#   scale_colour_gradientn(colours = heat.colors(10),
#                          limits = c(25,max(test$sci_water_temp, na.rm = TRUE)))

p2 <- ggplot(data=chunk,
       aes(x=m_present_time,
           y=m_depth,
           z=sci_rbrctd_temperature_00)) +
  geom_point(
    aes(color = sci_rbrctd_temperature_00)
  ) +
  scale_y_reverse() +
  scale_colour_gradientn(colours = heat.colors(10),
                         limits = c(25,max(chunk$sci_rbrctd_temperature_00, na.rm = TRUE)))

p3 <- ggplot(data=test,
       aes(x=m_present_time,
           y=m_depth,
           z=sci_rbrctd_salinity_00)) +
  geom_point(
    aes(color = sci_rbrctd_salinity_00)
  ) +
  scale_y_reverse() +
  scale_colour_gradientn(colours = heat.colors(10),
                         limits = c(35.5,max(test$sci_rbrctd_salinity_00, na.rm = TRUE)))

# p4 <- ggplot(data=chunk,
#              aes(x=m_present_time,
#                  y=m_depth,
#                  z=sci_rbrctd_pressure_00)) +
#   geom_point(
#     aes(color = sci_rbrctd_pressure_00)
#   ) +
#   scale_y_reverse() +
#   scale_colour_gradientn(colours = heat.colors(10),
#                          limits = c(0,max(chunk$sci_rbrctd_pressure_00, na.rm = TRUE)))


grid.arrange(p1, p2, p3)


  #theme(legend.position="bottom") +
  theme_bw() + 
  ggtitle("Richness over time") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1998, 2020, 2)) +
  ylab("Number of taxa per haul") +
  geom_line(aes(color = "mean")) +
  facet_grid(season ~ system) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(strip.text.y = element_text(size = 20))