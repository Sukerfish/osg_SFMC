library(tidyverse)
library(seewave)
library(tuneR)

# test <- readWave("stella.wav")
# 
# out <- ggspectro(test)
# 
# spectro(test, wl=512, ovlp=75, osc=TRUE)
# 
# spectro(test, f=22050, wl=512, ovlp=75, collevels=seq(-40,0,0.5))
# 
# listen(test)
# 
# out + scale_fill_continuous(name="Amplitude\n(dB)\n", limits=c(-30,0), na.value="transparent")

pamInfo <- file.info(list.files(path = "/echos/PAM/",
                                full.names = TRUE)) %>%
  filter(size > 0)

pamList <- rownames(pamInfo) %>%
  basename()

for (i in pamList){
  
  print(i)
  
  inwave <- readWave(paste0("/echos/PAM/", i))
  
  png(paste0("/echos/spectrograms/", i, ".png"),
      width = 1920,
      height = 1080)
  
  out <- spectro(inwave, flog=TRUE, wl=512, ovlp=99, osc=TRUE)

  dev.off()
  # ggsave(filename = paste0(i, ".png"),
  #        plot = ggEcho,
  #        device = "png",
  #        path = "./pseudograms",
  #        width = 21,
  #        height = 9)

}
