### battery publishing ###

library(tidyverse)

#get deployed gliders
deployedGliders <- read.csv("/echos/deployedGliders.txt", 
                            sep = "",
                            header = FALSE)
colnames(deployedGliders)[1] = "Name"
colnames(deployedGliders)[2] = "ahrCap"

#only process "real" ones
deployedGliders <- deployedGliders %>%
  filter(!str_starts(Name,"#")) #remove any commented lines

#initialize list
gliders_live <- list()
for (i in deployedGliders$Name){
  
  #load latest live data file
  load(paste0("/echos/", i, "/glider_live.RData"))
  
  fileConn<-file(paste0("/echos/", i , "/battLeft.txt")) #open file connection
  writeLines(c(as.character(battLeft)), fileConn) #write battLeft var as char
  close(fileConn) #close it
  
}