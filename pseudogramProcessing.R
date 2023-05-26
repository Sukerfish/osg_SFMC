###build pseudograms for whole mission

source("./thebrewery/scripts/pseudogram.R")
library(ggplot2)
library(tidyverse)
library(egg)
library(scales)
library(png)
library(echogram)

gliderName <- "usf-stella"
load(paste0("/echos/", gliderName, "/glider_live.RData"))

logo <- grid::rasterGrob(readPNG("./www/Classic-Left-CMS-Stacked-1000.png"), interpolate = TRUE, 
                         width=unit(3,'in'),
                         x = unit(1,"npc"), y = unit(1,"npc"),
                         hjust = 6.4, vjust = 10)

### pseudogram plotting ####
for (i in echoListraw$value){

  print(i)
  # process into long format for plotting
  ehunk <- pseudogram(paste0("/echos/layers/", i, ".ssv"),
                      paste0("/echos/depths/", i, ".ssv"))
  
  ggEcho <- 
    ggplot(data = 
             ehunk,
           aes(x=m_present_time,
               y=p_depth,
               z=value)) +
    geom_point(
      aes(color = value),
      size = 6,
      pch = 15,
      na.rm = TRUE
    ) +
    scale_colour_gradientn(colours = c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
                                       "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28"),
                           limits = c(-75, -30)) +
    scale_y_reverse() +
    theme_bw() +
    labs(title = paste0(i, " Pseudogram"),
         y = "Depth (m)",
         x = "Date/Time (UTC)",
         colour = "dB") +
    theme(plot.title = element_text(size = 32),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.key = element_blank()) +
    guides(size="none") +
    scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M")) +
    annotation_custom(logo)
  
  ggsave(filename = paste0(i, ".png"),
         plot = ggEcho,
         device = "png",
         path = "./pseudograms",
         width = 21,
         height = 9)
  
}

### ek runtimes ###
ekRunList <- list()
for (i in echoListraw$value){
  
  print(i)
  # process into long format for plotting
  ehunk <- pseudogram(paste0("/echos/layers/", i, ".ssv"),
                      paste0("/echos/depths/", i, ".ssv"))
  
  times <- ehunk %>%
    select(m_present_time) %>%
    distinct() %>%
    arrange()
  
  ekRunList[[i]]$start <- min(times$m_present_time)
  ekRunList[[i]]$end <- max(times$m_present_time)
  
}

ekRun <- bind_rows(ekRunList, .id = "segment")
  
write.csv(ekRun, file = "ekRunM125.csv")

### raw ek ####

test <- read.EK_raw("/echos/raw/Stella_2HZ_200kHZ_128_80-Phase0-D20230329-T065818-1.raw")
range <- convertPower(test, frequency = NULL)
testing <- get_dgIdx(test)
testt <- xcvrConf(test, 92672)

testy <- get_RAW0(test, angles = FALSE)
ntr <-  readBin(test[529:532], 'integer', 1, 4, signed	= TRUE, endian = "little")
dgIdx <- get_dgIdx(raw)	
idx <- testing[testing$dgType == "RAW3", ]


#if (!inherits(raw, "raw"))
  raw <- read.EK_raw("/echos/raw/Stella_2HZ_200kHZ_128_80-Phase0-D20230329-T065818-1.raw")
dgIdx <- get_dgIdx(raw)	
idx <- dgIdx[dgIdx$dgType == "RAW3", ]

# number of transceivers
ntr <-  1#readBin(raw[529:532], 'integer', 1, 4, signed	= TRUE, endian = "little")

# number of pings
nraw <- nrow(idx)
npings <- ceiling(nraw/ntr) # using ceiling in case of missing pings

# determine the maximum number of depth samples
ns <- rep(NA, nraw) 
i <- idx$sdgD + 70
j <- i + 3 
for (p in 1:nraw)
  ns[p] <- readBin(raw[i[p]:j[p]], 'integer', 1, 4, endian = "little")
ns <- max(ns)

# Define arrays for sample data, power, and angles
# *** need to fix differences in pulse lengths *****
sampleData <- array(NA, dim = c(npings, 21, ntr))
dimnames(sampleData)[2] <- list(c("pingTime", "channel", "mode", "transducerDepth",
                                  "frequency", "transmitPower", "pulseLength", "bandWidth", "sampleInterval", 
                                  "soundVelocity", "absorptionCoeff", "heave", "roll", "pitch", "temperature", 
                                  "trawlUpperDepthValid", "trawlOpeningValid", "trawlUpperDepth", "trawlOpening",
                                  "offset", "count"))
Pr <- array(NA, dim=c(ns, npings, ntr))
if (angles == TRUE)
  Ang <- array(NA, dim=c(ns, npings, ntr, 2))  

len <- c(8, 2, 1, 1, rep(4, 12), 2, 2, rep(4, 4))
# get sample data
for (tr in 1:ntr){
  tridx <- idx$sdgD[seq(tr, nraw, ntr)]
  for (j in 1:npings){
    ini <- tridx[j]
    i <- c(ini, ini + cumsum(len))[1:length(len)]
    k <- i + len - 1
    chan <- readBin(raw[i[2]:k[2]], 'integer', 1, 2, endian = "little")
    sampleData[j, 1, chan] <- dgTime(raw, i[1])
    sampleData[j, 2, chan] <- chan
    mode <- readBin(raw[i[3]:k[4]], "integer", 2, 1, endian = "little")
    mode <- 256 * mode[2] + mode[1]
    sampleData[j, 3, chan] <- mode
    sampleData[j, 4:15, chan] <- readBin(raw[i[5]:k[16]], 'double', 12, 4, endian = "little")
    sampleData[j, 16:17, chan] <- readBin(raw[i[17]:k[18]], "integer", 2, 2, endian = "little")
    sampleData[j, 18:19, chan] <- readBin(raw[i[19]:k[20]], "double", 2, 4, endian = "little")
    sampleData[j, 20, chan] <- readBin(raw[i[21]:k[21]], 'integer', 1, 4, endian = "little")
    count <- readBin(raw[i[22]:k[22]], 'integer', 1, 4, endian = "little")
    sampleData[j, 21, chan] <- count
    
    # Received Power
    ip <- ini + 80
    ipc <- ini + count * 2; 
    power <- readBin(raw[ip:ipc], 'integer', count, 2, endian = "little")
    d <- ns - length(power)
    if (length(power) < ns)
      power <- c(power, rep(NA, d))
    Pr[ , j, chan] <- power
    
    # Angles
    if (angles == TRUE & mode > 1){
      ipa <- ipc + 1 + count * 4
      angl  <- readBin(raw[(ipc + 1):ipa], 'integer', count * 2, 1, endian = "little")
      angl <- t(matrix(angl, nrow = 2))
      along <- as.vector(angl[, 1]) 
      athw <- as.vector(angl[, 2])
      d <- ns - length(along)
      if (length(along) < ns){
        along <- c(along, rep(NA, d))
        athw <- c(athw, rep(NA, d))
      }   
      Ang[ , j, chan, 1] <- along
      Ang[ , j, chan, 2] <- athw
    }
  }
}
Pr <- Pr * 10 * log10(2)/256

freqs <- as.vector(unique(sampleData[ , 'frequency', ]))
dimnames(sampleData)[3] <- list(freqs)
dimnames(Pr)[3] <- list(freqs)
ans <- list(sampleData = sampleData, Pr = Pr)

if (angles == TRUE & mode > 1){
  dimnames(Ang)[3:4] <- list(freqs, c("alongship", "athwartship"))
  ans <- list(sampleData = sampleData, Pr = Pr, angles = Ang)
}		
ans
