library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(scales)
#library(echogram)

#test <- palette.echogram(Svthr = -75, Svmax = -35, col.sep = 1, scheme = "echov", visu = FALSE)

raw <- read.csv("./The Brewery/pseudograms/velocities/usf-stella-2023-059-1-251.ssv",
                sep="", #whitespace as delimiter
                #skip=2,
                header = FALSE)

rawDepth <- read.csv("./The Brewery/pseudograms/depths/usf-stella-2023-059-1-251.ssv",
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
  mutate(p_depth = i_depth + offset))

#plot
ggplot(data = 
         bigLong,
       aes(x=m_present_time,
           y=p_depth,
           z=value)) +
  # geom_tile(aes(
  #   color = value)
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
  scale_colour_viridis_c(limits = c(min(bigLong$value), max(bigLong$value)),
                         option = "C"
                         ) +
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
  theme(plot.title = element_text(size = 32)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))
