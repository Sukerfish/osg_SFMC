library(tidyverse)
library(ggplot2)
library(lubridate)

raw <- read.csv("usf-stella-2023-059-1-194.ssv",
                sep="", #whitespace as delimiter
                #skip=2,
                header = FALSE)

rawDepth <- read.csv("./depths/usf-stella-2023-059-1-194.ssv",
                sep="", #whitespace as delimiter
                skip=17,
                header = FALSE)

df <- as.data.frame(raw) %>%
  rename(m_present_time = V1)
df$m_present_time <- as_datetime(df$m_present_time)

ef<- as.data.frame(rawDepth) %>%
  rename(m_present_time = V2) %>%
  rename(m_depth = V1)
ef$m_present_time <- as_datetime(ef$m_present_time)

bigLong <- pivot_longer(df, !c(m_present_time))


ggplot(data = 
         bigLong,
       aes(x=m_present_time,
           y=m_depth,
           z=decibels)) +
  geom_point(
    aes(color = decibels),
    na.rm = TRUE
  ) +
  #coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
  #geom_hline(yintercept = 0) +
  scale_y_reverse() +
  scale_colour_viridis_c(limits = c(input$min, input$max)) +
  # geom_point(data = filter(chunk(), m_water_depth > 0),
  #            aes(y = m_water_depth),
  #            size = 0.1,
  #            na.rm = TRUE
  # ) +
  theme_bw() +
  # labs(title = paste0(missionNum, " Science Data"),
  #      y = "Depth (m)",
  #      x = "Date") +
  theme(plot.title = element_text(size = 32)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12))
})