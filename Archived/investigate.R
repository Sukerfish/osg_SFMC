library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(cowplot)

load("M116.RData")

# format present time for easier use
nohead$m_present_time <- as_datetime(nohead$m_present_time)

# establish glider motion based on depth rate
nohead <- nohead %>%
  mutate(status = if_else(m_avg_depth_rate > 0, "dive", "climb")) %>%
  #fill in the blanks - works as long as ordered by present time
  fill(status)

# establish list of vars of interest
vars <-
  c("m_present_time",
    "status",
    "m_depth",
    "sci_rbrctd_conductivity_00",
    "sci_rbrctd_salinity_00",
    "sci_rbrctd_temperature_00",
    "sci_flbbcd_cdom_units",
    "sci_flbbcd_chlor_units",
    "sci_oxy4_oxygen"
    )

# separate out specific time of interest and filter by dive/climb
chunk <- nohead %>%
  filter(m_present_time >= "2022-06-29" & m_present_time < "2022-07-04") %>%
  filter(status == "dive") %>%
  subset(select = c(vars)) %>%
  # in case of bad yo (specifically July 3)
  filter(sci_rbrctd_conductivity_00 > 0)

p1 <- ggplot(data=chunk,
       aes(x=m_present_time,
           y=m_depth,
           z=sci_rbrctd_conductivity_00)) +
  geom_point(
    aes(color = sci_rbrctd_conductivity_00)
  ) +
  scale_y_reverse() +
  scale_color_viridis_c(option = "plasma", na.value = NA)
  
p2 <- ggplot(data=chunk,
       aes(x=m_present_time,
           y=m_depth,
           z=sci_rbrctd_temperature_00)) +
  geom_point(
    aes(color = sci_rbrctd_temperature_00)
  ) +
  scale_y_reverse() +
  scale_color_viridis_c(option = "plasma", na.value = NA)

p3 <- ggplot(data=chunk,
       aes(x=m_present_time,
           y=m_depth,
           z=sci_rbrctd_salinity_00)) +
  geom_point(
    aes(color = sci_rbrctd_salinity_00)
  ) +
  scale_y_reverse() +
  scale_color_viridis_c(option = "plasma", na.value = NA)

#collect ctd plots
plot_ctd <- plot_grid(p1, p2, p3,
                      align = "v", 
                      nrow = 3
                      )

#make ctd plot tile
title <- ggdraw() + 
  draw_label(
    "M116 raw CTD dive data",
    fontface = 'bold',
    size = 18,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

#merge as one plot
plot_grid(
  title, plot_ctd,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

# export size 1400 x 750

#MORE PLOTS
p4 <- ggplot(data=chunk,
             aes(x=m_present_time,
                 y=m_depth,
                 z=sci_flbbcd_cdom_units)) +
  geom_point(
    aes(color = sci_flbbcd_cdom_units)
  ) +
  scale_y_reverse() +
  scale_color_viridis_c(option = "plasma", limits = c(0, 2), na.value = NA)

p5 <- ggplot(data=chunk,
             aes(x=m_present_time,
                 y=m_depth,
                 z=sci_flbbcd_chlor_units)) +
  geom_point(
    aes(color = sci_flbbcd_chlor_units)
  ) +
  scale_y_reverse() +
  scale_color_viridis_c(option = "plasma", limits = c(0, 2), na.value = NA)

p6 <- ggplot(data=chunk,
             aes(x=m_present_time,
                 y=m_depth,
                 z=sci_oxy4_oxygen)) +
  geom_point(
    aes(color = sci_oxy4_oxygen)
  ) +
  scale_y_reverse() +
  scale_color_viridis_c(option = "plasma", na.value = NA)



#collect plots
plot_flctd <- plot_grid(p4, p5, p3,
                      align = "v", 
                      nrow = 3
)

#make plot tile
title <- ggdraw() + 
  draw_label(
    "M116 raw FL/CTD dive data",
    fontface = 'bold',
    size = 18,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

#merge as one plot
plot_grid(
  title, plot_flctd,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

# export sizesci_flbbcd_chlor_units 1400 x 750