library(tidyverse)
library(seewave)
library(tuneR)
library(lubridate)

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

#z = "20230328T152400_4407951451798164_2.0.wav"
z = "20241123T200100_4434821331994775_2.0.wav"
dirz ="PAM146"

pamInfo <- file.info(list.files(path = paste0("D:/", dirz, "/"),
                                full.names = TRUE)) %>%
  filter(size > 0)

pamList <- rownames(pamInfo) %>%
  basename()

iters <- as.numeric(length(pamList))
#init <- numeric(iters)
#end <- numeric(iters)

pb <- txtProgressBar(min = 0, 
                     max = iters, 
                     initial = 0, 
                     style = 3,
                     #width = iters,
                     char = "=") 

for (i in 1:iters){
  #init[i] <- Sys.time()
  #print(i)
  if(file.exists(paste0("D:/", dirz, "/spectro/", pamList[i], ".png"))) {
    next
  }
  
  inwave <- readWave(paste0("D:/", dirz, "/", pamList[i]))
  #inwave <- readWave(paste0(i))
  
  png(paste0("D:/", dirz, "/spectro/", pamList[i], ".png"),
      width = 1920,
      height = 1080)
  
  # png(paste0(i, ".png"),
  #     width = 1920,
  #     height = 1080)
  
  rewave <- resamp(inwave, g = 2000)
  fwave <- ffilter(rewave, f = 2000, from = 10, to=1000, fftw = TRUE)
  
  out <- spectro(fwave,f=2000,wl=1024,ovlp=85,fftw = TRUE)
  
  #out <- ggspectro(rewave, f = 2000, flog=TRUE, wl=512)

  dev.off()
  #end[i] <- Sys.time()
  setTxtProgressBar(pb, as.numeric(i))
  #time <- round(seconds_to_period(sum(end - init)), 0)

  # Estimated remaining time based on the
  # mean time that took to run the previous iterations
  # est <- iters * (mean(end[end != 0] - init[init != 0])) - time
  # remainining <- round(seconds_to_period(est), 0)
  # 
  # cat(paste(" // Execution time:", time,
  #           " // Estimated time remaining:", remainining), "")
  
  
}
close(pb)

inwave <- readWave(paste0(z))

rewave <- resamp(inwave, g = 2000)
fwave <- ffilter(rewave, f = 2000, from = 10, to=1000, fftw = TRUE)
d<-ffilter(inwave,from=30,to=2000,rescale=TRUE, fftw = TRUE)
spectro(fwave,f=2000,wl=1024,ovlp=85,fftw = TRUE)
spectro(fwave,f=4000)
(v <- ggspectro(fwave, f = 4000, wl=1024, ovlp=50) + 
  geom_tile(aes(fill = amplitude)) + 
  stat_contour() + 
  scale_fill_viridis_c())

#Soldevilla parameters
rewave <- resamp(inwave, g = 600)
fwave <- ffilter(rewave, f = 600, from = 10, to=300, fftw = TRUE)
spectro(fwave,f=600,wl=1024,ovlp=50,fftw = TRUE)
(v <- ggspectro(fwave, f = 600, wl=1024, ovlp=50) + 
    geom_tile(aes(fill = amplitude)) + 
    stat_contour() + 
    scale_fill_viridis_c())


data(tico)
## Not run: 
require(ggplot2)
## first layer
v <- ggspectro(tico, f = 2000, wl=512, ovlp=50)
summary(v)
## using geom_tile ##
v + geom_tile(aes(fill = amplitude)) + stat_contour()
## coordinates flip (interest?)
v + geom_tile(aes(fill = amplitude)) + stat_contour() + coord_flip()
## using stat_contour ##
# default (not nice at all)
v + stat_contour(geom="polygon", aes(fill=..level..))
# set up to 30 color levels with the argument bins
(vv <- v + stat_contour(geom="polygon", aes(fill=..level..), bins=30))
# change the limits of amplitude and NA values as transparent
vv + scale_fill_continuous(name="Amplitude\n(dB)\n", limits=c(-30,0), na.value="transparent")
# Black-and-white theme
(vv + scale_fill_continuous(name="Amplitude\n(dB)\n", limits=c(-30,0),
                            na.value="transparent", low="white", high="black") + theme_bw())
# Other colour scale (close to spectro() default output)
v + stat_contour(geom="polygon", aes(fill=..level..), bins=30)
+ scale_fill_gradientn(name="Amplitude\n(dB)\n", limits=c(-30,0),
                       na.value="transparent", colours = spectro.colors(30))

## End(Not run)


## Not run: 
data(tico)
data(pellucens)
# simple plots
spectro(rewave,f=22050)
spectro(inwave, f=22050, flim=c(0,2))
spectro(tico,f=22050,osc=TRUE)
spectro(tico,f=22050,scale=FALSE)
spectro(tico,f=22050,osc=TRUE,scale=FALSE)
# change the dB scale by setting a different dB reference value (20microPa)
spectro(rewave, f=22050, dBref=2*10e-5)
# unnormalised spectrogram with a linear amplitude scale
spectro(tico, dB=NULL, norm=FALSE, scale=FALSE)
# manipulating wl
op<-par(mfrow=c(2,2))
spectro(rewave,wl=256,scale=FALSE)
title("wl = 256")
spectro(tico,f=22050,wl=512,scale=FALSE)
title("wl = 512")
spectro(tico,f=22050,wl=1024,scale=FALSE)
title("wl = 1024")
spectro(tico,f=22050,wl=4096,scale=FALSE)
title("wl = 4096")
par(op)
# vertical zoom using flim
spectro(tico,f=22050, flim=c(2,6))
spectro(tico,f=22050, flimd=c(2,6))
# a full plot
pellu2<-cutw(pellucens,f=22050,from=1,plot=FALSE)
spectro(rewave,ovlp=85,zp=16,osc=TRUE,
        cont=TRUE,contlevels=seq(-30,0,20),colcont="red",
        lwd=1.5,lty=2,palette=reverse.terrain.colors)
# black and white spectrogram 
spectro(rewave,ovlp=85,zp=16,
        palette=reverse.gray.colors.1)
# colour modifications
data(sheep)
spectro(sheep,f=8000,palette=temp.colors,collevels=seq(-115,0,1))
spectro(pellu2,f=22050,ovlp=85,zp=16,
        palette=reverse.cm.colors,osc=TRUE,colwave="orchid1") 
spectro(rewave,ovlp=85,zp=16,osc=TRUE,palette=reverse.heat.colors,
        colbg="black",colgrid="white", colwave="white",colaxis="white",collab="white")

## End(Not run)


a<-noisew(f=8000,d=1)
# low-pass
b<-ffilter(a,f=8000,to=1500)
spectro(b,f=8000,wl=512)
# high-pass
c<-ffilter(a,f=8000,from=2500)
spectro(c,f=8000,wl=512)
# band-pass
d<-ffilter(a,f=8000,from=1000,to=2000)
spectro(d,f=8000,wl=512)
# band-stop
e<-ffilter(a,f=8000,from=1500,to=2500,bandpass=FALSE)
spectro(e,f=8000,wl=512)
# custom
myfilter1<-rep(c(rep(0,64),rep(1,64)),4)
g<-ffilter(a,f=8000,custom=myfilter1)
spectro(g,f=8000)


inwave <- readWave(paste0(z))
#inwave <- readWave(paste0(i))

png(paste0(z, ".png"),
    width = 1920,
    height = 1080)

# png(paste0(i, ".png"),
#     width = 1920,
#     height = 1080)

rewave <- resamp(inwave, g = 2000)
fwave <- ffilter(rewave, f = 2000, from = 10, to=1000, fftw = TRUE)

out <- spectro(fwave,f=2000,wl=1024,ovlp=85,fftw = TRUE)

#out <- ggspectro(rewave, f = 2000, flog=TRUE, wl=512)

dev.off()
