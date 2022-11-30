library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)

load("M116.RData")

# vars <- 
#   c("sci_rbrctd_salinity_00", "m_water_depth", "m_present_time")

nohead$m_present_time <- as_datetime(nohead$m_present_time)

chunk <- nohead %>%
  filter(m_present_time >= "2022-07-01" & m_present_time < "2022-07-02") %>%
  mutate(status = if_else(m_avg_depth_rate > 0, "dive", "climb")) %>%
  fill(status) %>%
  filter(status == "dive")

test <- chunk %>%
  filter(!(is.na(sci_rbrctd_salinity_00) | is.na(sci_rbrctd_temperature_00)
         | is.na(m_depth)))

p1 <- ggplot(data=test,
       aes(x=m_present_time,
           y=m_depth,
           z=sci_rbrctd_conductivity_00)) +
  geom_point(
    aes(color = sci_rbrctd_conductivity_00)
  ) +
  scale_y_reverse() +
  scale_colour_gradientn(colours = heat.colors(10),
                         limits = c(55,max(test$sci_rbrctd_conductivity_00, na.rm = TRUE)))
  
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