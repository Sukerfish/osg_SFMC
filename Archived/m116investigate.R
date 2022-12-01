library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(cowplot)

library(marelac)
library(plot3D)

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
  filter(m_present_time >= "2022-07-03" & m_present_time < "2022-07-04") %>%
  filter(status == "dive") %>%
  subset(select = c(vars)) %>%
  # in case of bad yo (specifically July 3)
  filter(sci_rbrctd_conductivity_00 > 0)

#### generic plots ####
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

#### T-S plots ####

mint = min(chunk$sci_rbrctd_temperature_00, na.rm = TRUE)
maxt = max(chunk$sci_rbrctd_temperature_00, na.rm = TRUE)
mins = min(chunk$sci_rbrctd_salinity_00, na.rm = TRUE)
maxs = max(chunk$sci_rbrctd_salinity_00, na.rm = TRUE)

salC  <- seq(from=mins,to=maxs,length.out = 156)
tempC <- seq(from=mint,to=maxt,length.out = 156)

sigma.c<-outer(salC,tempC,FUN = function(S,t)sw_dens(S = S, t = t)-1000)
sigma.c

#png(file = "ts_diagram.png",width = 15,res=500,pointsize = 12,bg="white")

jpeg(filename = "ts_diagram.jpeg",
     width = 1920, 
     height = 1920, 
     units = "px", 
     pointsize = 12,
     #quality = 75,
     bg = "white", 
     res = 200)
par(mar=c(5,5,4,6))
contour2D(x=salC,y=tempC,z=sigma.c,lwd=2,main="M116 July 3 T-S plot",
          col="black",xlab=expression("Sanlinity(‰)"),ylab=expression("Temperature("*~degree*C*")"))

temp <- chunk$sci_rbrctd_temperature_00
sal  <- chunk$sci_rbrctd_salinity_00

# temp<-unlist(ts[‘temperatureSurface’],use.names = FALSE)
# sal<-unlist(ts[‘salinitySurface’],use.names = FALSE)

sigma_theta<-sw_dens(S=sal,t=temp)-1000
scatter2D(sal,temp,colvar = sigma_theta,pch=16,cex=1.25,add=TRUE,
          clim = range(sigma.c),colkey = FALSE)
colkey(clim = range(sigma.c),dist = 0.005,side=4,add=TRUE,
       clab = expression("Density(kg"~m^-3*")"),col.clab = "black",
side.clab = 4,line.clab = 2.5,length = 1,width = 0.8,
col.axis = "black",col.ticks = "black",cex.axis = 0.9)

dev.off()