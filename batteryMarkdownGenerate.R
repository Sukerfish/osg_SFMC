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
  #load latest live data file into blank environment
  load(paste0("/echos/", i, "/glider_live.RData"),  gliders <- new.env())
  
  if (gliders$ahrCap$ahrCap > 0){
    msg <- envelope() %>%
      emayili::render("/echos/batteryMarkdown.Rmd", .envir = gliders) %>%
      subject(paste0("Daily summary for ", as.character(i)))
    
    capture.output(print(msg, details = TRUE), file = paste0("/echos/", i, "/summary.html"))
  } else {
    msg <- envelope() %>%
      emayili::render("/echos/batteryMarkdownNoAmp.Rmd", .envir = gliders) %>%
      subject(paste0("Daily summary for ", as.character(i)))
    
    capture.output(print(msg, details = TRUE), file = paste0("/echos/", i, "/summary.html"))
  }
  
}