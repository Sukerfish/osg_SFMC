library(tidyverse)
library(dplyr)
library(seacarb)
library(svglite)
library(egg)
library(lubridate)

source("/srv/shiny-server/thebrewery/scripts/ssv_to_df.R")

#https://rdrr.io/github/AustralianAntarcticDivision/ZooScatR/src/R/soundvelocity.R
c_Coppens1981 <- function(D,S,T){
  t <- T/10
  D = D/1000
  c0 <- 1449.05 + 45.7*t - 5.21*(t^2)  + 0.23*(t^3)  + (1.333 - 0.126*t + 0.009*(t^2)) * (S - 35)
  c <- c0 + (16.23 + 0.253*t)*D + (0.213-0.1*t)*(D^2)  + (0.016 + 0.0002*(S-35))*(S- 35)*t*D
  return(c)
}

#https://rdrr.io/cran/wql/man/ec2pss.html
ec2pss <-
  function (ec, t, p = 0) {
    # Define conductivity ratio
    R <- ec/42.914
    
    # Estimate temperature correction (valid for -2 < t < 35)
    c <- c(0.6766097, 0.0200564, 0.0001104259, -6.9698e-07, 1.0031e-09)
    rt <- c[1] + c[2] * t + c[3] * t^2 + c[4] * t^3 + c[5] * t^4
    
    # Estimate pressure correction (validity range varies with t and S)
    d <- c(0.03426, 0.0004464, 0.4215, -0.003107)
    e <- c(2.07e-05, -6.37e-10, 3.989e-15)
    Rp <- 1 + p * (e[1] + e[2] * p + e[3] * p^2)/(1 + d[1] * t + 
                                                    d[2] * t^2 + (d[3] + d[4] * t) * R)
    
    # Estimate salinity (valid for 2 < S < 42 and -2 < t < 35).       
    Rt <- R/(Rp * rt)
    a <- c(0.008, -0.1692, 25.3851, 14.0941, -7.0261, 2.7081)
    b <- c(5e-04, -0.0056, -0.0066, -0.0375, 0.0636, -0.0144)
    ft <- (t - 15)/(1 + 0.0162 * (t - 15))
    S <- a[1] + a[2] * Rt^0.5 + a[3] * Rt + a[4] * Rt^1.5 + a[5] * 
      Rt^2 + a[6] * Rt^2.5 + ft * (b[1] + b[2] * Rt^0.5 + b[3] * 
                                     Rt + b[4] * Rt^1.5 + b[5] * Rt^2 + b[6] * Rt^2.5)
    
    # Estimate salinity correction for S < 2
    x <- 400 * Rt
    y <- 100 * Rt
    ifelse(S >= 2, S, S - a[1]/(1 + 1.5 * x + x^2) - b[1] * ft/(1 + 
                                                                  y^0.5 + y + y^1.5))
  }

scienceList_liveInfo <- file.info(list.files(path = "/echos/science/",
                                             full.names = TRUE)) %>%
  filter(size > 0)

scienceList_live <- rownames(scienceList_liveInfo) %>%
  basename()

flightList_liveInfo <- file.info(list.files(path = "/echos/flight/",
                                            full.names = TRUE)) %>%
  filter(size > 0)

flightList_live <- rownames(flightList_liveInfo) %>%
  basename()

flightTotal <- length(flightList_live)
#if flightList changed ... then ... do df creation

flist <- list()
slist <- list()
for (i in flightList_live) {
  flist[[i]] <- ssv_to_df(paste0("/echos/flight/", i))
}
for (j in scienceList_live) {
  slist[[j]] <- ssv_to_df(paste0("/echos/science/", j))
}

fdf <- bind_rows(flist, .id = "segment") %>%
  filter(m_present_time > 1677646800)

sdf <- bind_rows(slist, .id = "segment") %>%
  filter(sci_m_present_time > 1677646800) %>%
  mutate(m_present_time = sci_m_present_time)

gliderdf <- fdf %>%
  select(!c(segment)) %>%
  full_join(sdf) %>%
  #select(!c(segment)) %>%
  arrange(m_present_time) %>%
  fill(segment, .direction = "downup") %>%
  mutate(osg_salinity = ec2pss(sci_water_cond*10, sci_water_temp, sci_water_pressure*10)) %>%
  mutate(osg_theta = theta(osg_salinity, sci_water_temp, sci_water_pressure)) %>%
  mutate(osg_rho = rho(osg_salinity, osg_theta, sci_water_pressure)) %>%
  mutate(osg_depth = p2d(sci_water_pressure*10, lat=30)) %>%
  mutate(osg_soundvel1 = c_Coppens1981(osg_depth,
                                       osg_salinity,
                                       sci_water_temp))

#pull out science variables
scivarsLive <- sdf %>%
  select(!(c(segment, m_present_time))) %>%
  colnames()

#pull out flight variables
flightvarsLive <- fdf %>%
  select(!(c(segment))) %>%
  #select(!starts_with("sci")) %>%
  colnames()

endDateLive <- max(gliderdf$m_present_time)

##### dashboard calculations #####
gliderdfChunk <- gliderdf %>%
  filter(m_present_time >= endDateLive-14400)

ahrCap <- 550
ahrUsed <- max(gliderdf$m_coulomb_amphr_total, na.rm = TRUE)
ahrLeft <- ahrCap-ahrUsed
pwr3day <- gliderdf %>%
  select(m_present_time, m_coulomb_amphr_total) %>%
  filter(m_present_time >= endDateLive - 259200,
         m_coulomb_amphr_total > 0)
pwr1day <- pwr3day %>%
  filter(m_present_time >= endDateLive - 86400)

ahr3day <- (max(pwr3day$m_coulomb_amphr_total)-min(pwr3day$m_coulomb_amphr_total))/(as.numeric(max(pwr3day$m_present_time))-as.numeric(min(pwr3day$m_present_time)))*86400
ahr1day <- (max(pwr1day$m_coulomb_amphr_total)-min(pwr1day$m_coulomb_amphr_total))/(as.numeric(max(pwr1day$m_present_time))-as.numeric(min(pwr1day$m_present_time)))*86400
ahrAllday <- (max(gliderdf$m_coulomb_amphr_total, na.rm = TRUE)-min(gliderdf$m_coulomb_amphr_total, na.rm = TRUE))/(as.numeric(max(gliderdf$m_present_time, na.rm = TRUE))-as.numeric(min(gliderdf$m_present_time, na.rm = TRUE)))*86400

LDmin <- min(gliderdfChunk$m_leakdetect_voltage, na.rm = TRUE)
battLeft <- (ahrLeft/ahrCap)*100

#### plots for carousel ####
bats <- gliderdf %>%
  select(c(m_present_time, m_battery)) %>%
  filter(m_battery > 0) %>%
  mutate(day = floor_date(m_present_time,
                          unit = "days")) %>%
  group_by(day) %>%
  mutate(meanBatt = mean(m_battery)) %>%
  #select(c(day, meanBatt)) %>%
  distinct(day, meanBatt)

battLive <- ggplot(
  data = 
    bats,
  aes(x=day,
      y=meanBatt,
  )) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  theme_bw() +
  labs(title = "Daily Voltage Average",
       y = "Battery (V)",
       x = "Date") +
  theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))

vars <- c("m_leakdetect_voltage", "m_leakdetect_voltage_forward", "m_leakdetect_voltage_science")

leaks <- gliderdf %>%
  select(c(m_present_time, any_of(vars))) %>%
  filter(m_leakdetect_voltage > 0) %>%
  filter(m_present_time >= endDateLive-14400) %>%
  pivot_longer(cols = any_of(vars))

leakLive <- ggplot(
  data = 
    leaks,
  aes(x=m_present_time,
      y=value,
      color = name
  )) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  theme_bw() +
  labs(title = "LD last 4hrs",
       y = "Voltage",
       x = "Date") +
  theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.position   =  "bottom")

roll <- gliderdf %>%
  select(c(m_present_time, m_roll)) %>%
  filter(!is.na(m_roll > 0)) %>%
  mutate(day = floor_date(m_present_time,
                          unit = "days")) %>%
  group_by(day) %>%
  mutate(meanRoll = mean(m_roll)) %>%
  #select(c(day, meanBatt)) %>%
  distinct(day, meanRoll)

rollLive <- ggplot(
  data = 
    roll,
  aes(x=day,
      y=meanRoll*180/pi,
  )) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  scale_y_continuous(limits = symmetric_range) +
  theme_bw() +
  labs(title = "Daily Roll Average",
       y = "Roll (deg)",
       x = "Date") +
  theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))

livePlots <- list(
  leakLive, battLive, rollLive
)

save(gliderdf, scivarsLive, flightvarsLive,
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
     file = "/echos/usf-stella/glider_live.RData")