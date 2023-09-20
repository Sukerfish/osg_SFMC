load("M133_usf-sam.RData")

library(tidyverse)
library(lubridate)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

theme_osg <- function(){ 
  theme_bw() %+replace%
    theme(
      plot.title = element_text(hjust = 0,
                                size = 32),
      plot.subtitle = element_text(hjust = 0,
                                   size = 16),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12),
      plot.caption = element_markdown(hjust = 1),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    )
}

# df <- gliderdf %>%
#   select(m_present_time, sci_suna_nitrate_concentration, osg_depth) %>%
#   filter(!is.na(sci_suna_nitrate_concentration))

spring <- interval(ymd_hm("2023-09-04T03:15"), ymd_hm("2023-09-04T03:54"))

df <- gliderdf %>%
  select(m_present_time, osg_salinity, i_lat, i_lon, m_water_depth, osg_depth) %>%
  filter(!is.na(osg_salinity)) %>%
  filter(m_present_time %within% spring) %>%
  filter(osg_depth > 1)

salPlot <-ggplot(data = 
         df,#dynamically filter the sci variable of interest
       aes(x=m_present_time,
           y=osg_depth,
           color= osg_salinity)
       ) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
 # coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
  #geom_hline(yintercept = 0) +
  scale_y_reverse() +
  scale_colour_viridis_c() +
  geom_point(data = filter(df, m_water_depth > 0),
             aes(y = m_water_depth),
             size = 0.1,
             na.rm = TRUE,
             color = "black"
  ) +
  theme_bw() +
  labs(title = paste0("M133", " Salinity"),
       y = "Depth (m)",
       x = "Date") +
  theme(plot.title = element_text(size = 32)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12))

# world <- ne_countries(scale = "medium", returnclass = "sf")
latitude <- as.numeric(c("27.7938", "27.8287", "27.86", "27.8253"))
longitude <- as.numeric(c("-84.2426", "-84.2542", "-84.1608", "-84.1542"))
rhombus <- data.frame(latitude, longitude)
rhomSF <- rhombus %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

p <- ggplot() +
  geom_sf(data = world) +
  geom_sf(data = rhomSF) +
  coord_sf(xlim = range(rhombus$longitude, na.rm = TRUE), 
           ylim = range(rhombus$latitude, na.rm = TRUE), 
           expand = TRUE)+
  geom_point(data = df, 
            aes(x = i_lon, y = i_lat, color = m_present_time),
            alpha = 0.3, 
            shape = 16, 
            size = 3) +
  theme_osg() 

out <- wrap_plots(salPlot, p)

# ggsave(filename = "springSite.png",
#        plot = out,
#        device = "png",
#        path = "./COMIT",
#        width = 16,
#        height = 9)
