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
  
  if (ahrCap$ahrCap > 0){
    recoDays <- ((ahrCap$ahrCap*.9)-ahrUsed)/ahr3day #calculate days til 10% abort level
    
    fileConn<-file(paste0("/echos/", i , "/battLeft.txt")) #open file connection
    writeLines(c(paste0("Subject: Daily summary for ", as.character(i)), #write email subject
                 paste0("Approximate time until 10% abort level: ", format(round(recoDays, 2), nsmall = 2), " days (calculated with 72hr usage)"), #write recoDays var as char
                 paste0("Total mission amphr usage: ", format(round(ahrAllday, 2), nsmall = 2), " amphr/day"),
                 paste0("Last 24hr amphr usage: ", format(round(ahr1day, 2), nsmall = 2), " amphr/day"),
                 paste0("Last 72hr amphr usage: ", format(round(ahr3day, 2), nsmall = 2), " amphr/day")
                 ), 
               sep = "\n",
               fileConn)
    close(fileConn) #close it
  } else {
  
  fileConn<-file(paste0("/echos/", i , "/battLeft.txt")) #open file connection
  writeLines(c(paste0("Subject: Daily summary for ", as.character(i)), #write email subject
               paste0("Expected days of battery left at typical usage: ", format(round(battLeft), nsmall = 2))), #write battLeft var as char
             sep = "\n",
             fileConn)
  close(fileConn) #close it
  }
  
}