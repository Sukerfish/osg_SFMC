# source("./thebrewery/scripts/pseudogram.R")
# library(ggplot2)
library(tidyverse)
library(lubridate)
# library(egg)
# library(scales)

gliderName <- "usf-stella"
gliderdfFull <- readRDS(paste0("/echos/", gliderName, "/M125.rds"))
load(paste0("/echos/", gliderName, "/glider_live.RData"))

head <- read.csv(paste0("/echos/", gliderName, "/oilvol.ssv"),
                 sep="", #whitespace as delimiter
                 skip=923,
                 nrows=1)

raw <- read.csv(paste0("/echos/", gliderName, "/oilvol.ssv"),
                sep="", #whitespace as delimiter
                skip=926,
                header = FALSE)

colnames(raw) <- colnames(head)

pumping <- raw %>%
  mutate(m_present_time = as_datetime(m_present_time)) #convert to POSIXct

pumping$m_present_time <- as_datetime(floor(seconds(pumping$m_present_time)))

timedelta <- gliderdfFull %>%
  select(m_present_time, sci_m_present_time) %>%
  mutate(scitime = as_datetime(sci_m_present_time)) %>%
  mutate(timedelta = m_present_time - scitime) %>%
  mutate(timenumber = as.numeric(timedelta))

### ek runtimes ###
ekRunList <- list()
for (i in echoListraw$value){
  
  print(i)
  # process into long format for plotting
  ehunk <- pseudogram(paste0("/echos/layers/", i, ".ssv"),
                      paste0("/echos/depths/", i, ".ssv"))
  
  times <- ehunk %>%
    select(m_present_time) %>%
    distinct() %>%
    arrange()
  
  ekRunList[[i]]$start <- min(times$m_present_time)
  ekRunList[[i]]$end <- max(times$m_present_time)
  
}

ekRun <- bind_rows(ekRunList, .id = "segment")

ekTiming <- ekRun %>%
  mutate(ekON = interval(start, end)) %>%
  select(segment, ekON)

pitchGlider <- gliderdfFull %>%
  select(m_present_time, c_battpos, sci_echodroid_sa) %>%
  fill(c_battpos) %>%
  mutate(batt_delta = c_battpos - lag(c_battpos, default = 0)) %>%
  filter(batt_delta != 0) %>%
  filter(!is.nan(sci_echodroid_sa))

noisyGlider <- gliderdfFull %>%
  select(m_present_time, c_battpos, sci_echodroid_sa, m_depth) %>%
  left_join(pumping) %>%
  fill(c_battpos) %>%
  fill(c_de_oil_vol) %>%
  mutate(batt_delta = c_battpos - lag(c_battpos, default = 0)) %>%
  mutate(pump_delta = c_de_oil_vol - lag(c_de_oil_vol, default = 0)) %>%
  filter(batt_delta != 0) %>%
  filter(!is.nan(sci_echodroid_sa)) %>%
  select(m_present_time, batt_delta, pump_delta, sci_echodroid_sa, m_depth)

write.csv(noisyGlider, file = "motorRuntimes.csv")

