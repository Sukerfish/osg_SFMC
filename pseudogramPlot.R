library(tidyverse)
library(ggplot2)
library(shiny)
library(leaflet)
library(ggtext)
#library(leaflet.extras)
library(sf)
#library(PlotSvalbard) #devtools::install_github("MikkoVihtakari/PlotSvalbard", upgrade = "never")
#library(patchwork)
#library(cowplot)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux?
library(scales)

#test <- palette.echogram(Svthr = -75, Svmax = -35, col.sep = 1, scheme = "echov", visu = FALSE)

raw <- read.csv("/echos/layers/usf-stella-2023-059-1-196.ssv",
                sep="", #whitespace as delimiter
                #skip=2,
                header = FALSE)

rawDepth <- read.csv("/echos/depths/usf-stella-2023-059-1-196.ssv",
                sep="", #whitespace as delimiter
                skip=17,
                header = FALSE)

#force as dataframe and rename
df <- as.data.frame(raw) %>%
  rename(m_present_time = V1)
#cutoff at seconds for clean merge
df$m_present_time <- as_datetime(floor(seconds(df$m_present_time)))

#tbd time correction
df <- df %>%
  mutate(m_present_time = m_present_time - 20)

#force as dataframe and rename
ef<- as.data.frame(rawDepth) %>%
  rename(m_present_time = V2) %>%
  rename(m_depth = V1)
#cutoff at seconds for clean merge
ef$m_present_time <- as_datetime(floor(seconds(ef$m_present_time)))

#m_depth interpolation (sbd sourced)
full.time <- with(ef,seq(m_present_time[1],tail(m_present_time,1),by=1)) #grab full list of timestamps
depth.zoo <- zoo(ef[1], ef$m_present_time) #convert to zoo
result <- na.approx(depth.zoo, xout = full.time) #interpolate

idepth <- fortify.zoo(result) %>% #extract out as DF
  rename(i_depth = m_depth) %>%
  rename(m_present_time = Index) %>%
  mutate(m_present_time = as_datetime(m_present_time))

#force both time sets to match (i.e., round to 1sec)
idepth$m_present_time <- as_datetime(floor(seconds(idepth$m_present_time)))

# SSV output for testing
# output <- idepth
# output$m_present_time <- as.numeric(output$m_present_time)
# write.table(output,
#             file =  "test.ssv",
#             quote = FALSE,
#             row.names = FALSE,
#             sep = " ")

#merge interpolated depth
bigLong <- pivot_longer(df, !c(m_present_time)) %>%
  left_join(idepth)

#build ping depth matrix
binList <- unique(bigLong$name)
binOffset <- data.frame(name = c(binList),
                   offset = c(seq(0, (length(binList)-1)*2.5, 2.5)))

#merge in ping depths and compute
bigLong <- as.data.frame(bigLong %>%
  left_join(binOffset) %>%
  mutate(p_depth = i_depth + offset)) %>%
  mutate(q_depth = round(p_depth, 1))

#plot
ggplot(data = 
         ef,
       aes(x=m_present_time,
           y=q_depth,
           z=value)) +
  # geom_tile(aes(
  #   color = value,
  #   size = 10)
  # ) +
  geom_point(
    aes(color = value),
    size = 6,
    pch = 15,
    na.rm = TRUE
  ) +
  #coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
  scale_y_reverse() +
  # scale_colour_manual(values = c(test$palette),
  #                     breaks = c(test$breaks))
  scale_colour_gradientn(colours = c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
                                     "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28"),
                         limits = c(-75, -35)) +
  # scale_colour_viridis_c(limits = c(min(bigLong$value), max(bigLong$value)),
  #                        option = "C"
  #                        ) +
  # geom_point(data = filter(chunk(), m_water_depth > 0),
  #            aes(y = m_water_depth),
  #            size = 0.1,
  #            na.rm = TRUE
  # ) +
  theme_bw() +
  labs(title = "EK",
       y = "Depth (m)",
       x = "Date/Time (UTC)",
       colour = "dB") +
  theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.key = element_blank(),
        plot.caption = element_markdown()) +
  guides(size="none") +
  scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M")) +
  facet_grid(. ~ segment, scales="free_x", space="free_x")
