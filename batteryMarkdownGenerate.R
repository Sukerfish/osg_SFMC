library(tidyverse)
library(emayili)

#get deployed gliders
deployedGliders <- read.csv("/echos/deployedGliders.txt", 
                            sep = "",
                            header = FALSE)
colnames(deployedGliders)[1] = "Name"
colnames(deployedGliders)[2] = "ahrCap"

#only process "real" ones
deployedGliders <- deployedGliders %>%
  filter(!str_starts(Name,"#")) #remove any commented lines

for (i in deployedGliders$Name){
  
  amps <- deployedGliders %>%
    filter(Name == i)
  
  #clear out potential objects
  invisible(rm(gliderdf, scivarsLive, flightvarsLive, toGliderList,
          ahrCap,
          ahrUsed,
          ahrLeft,
          pwr3day,
          pwr1day,
          ahr3day,
          ahr1day,
          ahrAllday,
          LDmin,
          battLeft,
          livePlots,
          msg))
  
  #load latest live data file
  load(paste0("/echos/", i, "/glider_live.RData"))
  
  if (amps$ahrCap > 0){
    msg <- envelope() %>%
      emayili::render("/echos/batteryMarkdown.Rmd") %>%
      subject(paste0("Daily summary for ", as.character(i)))
    
    capture.output(print(msg, details = TRUE), file = paste0("/echos/", i, "/summary.html"))
  } else {
    msg <- envelope() %>%
      emayili::render("/echos/batteryMarkdownNoAmp.Rmd") %>%
      subject(paste0("Daily summary for ", as.character(i)))
    
    capture.output(print(msg, details = TRUE), file = paste0("/echos/", i, "/summary.html"))
  }
  
}