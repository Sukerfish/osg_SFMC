# add live-generated echosounder plots to full mission RData

stella <- new.env()
full <- new.env()

load("glider_liveM134.RData", envir = stella)
load("./thebrewery/Data/M134_usf-stella.RData", envir = full)

gliderdf <- full$gliderdf
gliderName <- full$gliderName
echoListraw <- stella$echoListraw
fullehunk <- stella$fullehunk


#save(gliderdf, gliderName, echoListraw, fullehunk, file = "M199_usf-stella.RData")
