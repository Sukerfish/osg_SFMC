library(tidyverse)
library(dplyr)

source("./thebrewery/scripts/ssv_to_df.R")

scienceList_liveInfo <- file.info(list.files(path = "/echos/science/",
                                             full.names = TRUE)) %>%
  filter(size > 0)

scienceList_live <- rownames(scienceList_liveInfo) %>%
  basename()

flightList_liveInfo <- file.info(list.files(path = "/echos/flight/",
                                            full.names = TRUE)) %>%
  filter(size > 0)

flightList_live <- rownames(flightList_liveInfo) %>%
  basename()

flightTotal <- length(flightList_live)
#if flightList changed ... then ... do df creation

flist <- list()
slist <- list()
for (i in flightList_live) {
  flist[[i]] <- ssv_to_df(paste0("/echos/flight/", i))
}
for (j in scienceList_live) {
  slist[[j]] <- ssv_to_df(paste0("/echos/science/", j))
}

fdf <- bind_rows(flist, .id = "segment") %>%
  select(!c(segment)) %>%
  filter(m_present_time > 1677646800)

sdf <- bind_rows(slist, .id = "segment") %>%
  select(!c(segment)) %>%
  filter(sci_m_present_time > 1677646800) %>%
  mutate(m_present_time = sci_m_present_time)

glider_live <- list(science = sdf, flight = fdf)

saveRDS(glider_live, file = "glider_live.rds")