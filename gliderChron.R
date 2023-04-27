library(tidyverse)
# library(seacarb)
# library(svglite)
# library(egg)
# library(lubridate)
# library(ggplot2)
# library(scales)

source("./thebrewery/scripts/gliderLive.R")

deployedGliders <- read.csv("/echos/deployedGliders.txt", 
                            sep = "",
                            header = FALSE)
colnames(deployedGliders)[1] = "Name"
colnames(deployedGliders)[2] = "ahrCap"

gliders_live <- list()
for (i in deployedGliders$Name){
  df <- filter(deployedGliders, Name == i)
  
  gliderLive(df[1], df[2])
}