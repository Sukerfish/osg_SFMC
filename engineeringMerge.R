library(tidyverse)
library(lubridate)
library(seacarb)
library(osgUtils)
library(dplyr)
library(lubridate)

meta <- read.csv("/Users/Gymnothorax/Desktop/M123/test.ssv",
                 sep="", #whitespace as delimiter
                 nrows=14,
                 header = FALSE)

raw <- read.csv("/Users/Gymnothorax/Desktop/M123/test.ssv",
                sep="", #whitespace as delimiter
                skip=(as.numeric(meta[3,2])+3),
                header = FALSE)

head <- read.csv("/Users/Gymnothorax/Desktop/M123/test.ssv",
                sep="", #whitespace as delimiter
                skip=(as.numeric(meta[3,2])),
                nrows = 1,
                header = TRUE)

gliderdf_eng <- raw %>%
  `colnames<-`(colnames(head)) %>%
  #filter(!row_number() %in% c(1:2)) %>% #drop units and refresh time
  mutate(m_present_time = as_datetime(floor(seconds(m_present_time))))

load(paste0("./thebrewery/Data/", "M123_usf-jaialai", ".RData"))

gliderdf <- left_join(gliderdf, gliderdf_eng)

save(gliderdf, gliderName, file = paste0("./thebrewery/Data/","M123eng","_",gliderName,".RData"))

