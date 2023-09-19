#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras2)
library(serial)
library(sf)
library(geosphere)
#library(echogram)
#library(nmea)

source("./scripts/ssv_to_df.R")
source("./scripts/loadSSV.R")
source("./scripts/pseudogram.R")
source("./scripts/gotoLoad.R")
source("./scripts/gliderGPS_to_dd.R")

parse.HMS <- function(x, date, digits.secs=3) {
  options(digits.secs=digits.secs)
  strptime(paste(date, x), '%Y-%m-%d %H%M%OS', tz='UTC')
}

parse.latitude <- function(x) {
  (parts <- parse.named('(?<deg>[0-9]{2})(?<min>[0-9]{2}\\.[0-9]+),(?<dir>[NS])', x))
  decimal <- with(parts, as.numeric(deg) + as.numeric(min) / 60)
  decimal * ifelse(parts$dir == 'N', yes=1, no=-1)
}

parse.longitude <- function(x) {
  (parts <- parse.named('(?<deg>[0-9]{3})(?<min>[0-9]{2}\\.[0-9]+),(?<dir>[EW])', x))
  decimal <- with(parts, as.numeric(deg) + as.numeric(min) / 60)
  decimal * ifelse(parts$dir == 'E', yes=1, no=-1)
}

parse.quality <- function(x) {
  factor(x, levels=0:8, labels=c('invalid', 'GPS fix', 'DGPS fix', 'PPS fix', 'RT kinematic', 'float RTK', 'dead reckoning', 'manual input', 'simulation'))
}

parse.named <- function(pattern, x, ...) {
  matches <- regexpr(pattern, x, perl=TRUE, ...)
  first <- attr(matches, "capture.start")
  last <- first + attr(matches, "capture.length") - 1
  extract <- function(i) mapply(substring, x, first[,i], last[,i], USE.NAMES=FALSE)
  groupnames <- colnames(first)
  extracted <- sapply(groupnames, extract)
  reshaped <- matrix(extracted, ncol=length(groupnames))
  colnames(reshaped) <- groupnames
  data.frame(reshaped, stringsAsFactors=FALSE)
}

parse.IIGGA <- function(x, date) {
  pattern <- '\\$IIGGA,(?<time>[0-9.]+),(?<latitude>[0-9.]+,[NS]),(?<longitude>[0-9.]+,[EW]),(?<quality>[0-8]),'
  records <- parse.named(pattern, x)
  if (missing(date)) {
    warning("Defaulting to today's date for timestamps with no date.")
    date <- as.Date(Sys.time())
  }
  with(records, data.frame(
    datetime = parse.HMS(time, date=date),
    latitude = parse.latitude(latitude),
    longitude = parse.longitude(longitude),
    quality = parse.quality(quality)
  ))
}

parse.IIHDT <- function(x, date) {
  pattern <- '\\$IIHDT,(?<heading>[0-9.]+),T'
  records <- parse.named(pattern, x)
  with(records, data.frame(
    heading = heading
  ))
}

parse.IIVTG <- function(x) {
  pattern <- '\\$IIVTG,(?<headingT>[0-9.]+),T,(?<headingM>[0-9.]+),M,(?<speedN>[0-9.]+),N,(?<speedK>[0-9.]+),K,'
  records <- parse.named(pattern, x)
  with(records, data.frame(
    headingT = headingT,
    headingM = headingM,
    speedN = speedN,
    speedK = speedK
  ))
}

calculate_new_gps_position <- function(current_gps_df) {
  # Extract current GPS data
  current_latitude <- current_gps_df$latitude
  current_longitude <- current_gps_df$longitude
  speed_knots <- current_gps_df$speedN
  direction_degrees <- current_gps_df$headingT
  
  # Convert speed from knots to meters per second (m/s)
  speed_ms <- speed_knots * 0.514444  # 1 knot = 0.514444 m/s
  
  # Calculate the displacement vector in meters
  displacement <- geosphere::distVincentySphere(
    c(current_longitude, current_latitude),
    c(current_longitude + sin(direction_degrees * pi / 180) * (speed_ms * 10 / 1852),
      current_latitude + cos(direction_degrees * pi / 180) * (speed_ms * 10 / 1852))
  )
  
  # Calculate the new latitude and longitude
  new_coords <- geosphere::destPoint(
    p = c(current_longitude, current_latitude),
    b = direction_degrees,
    d = displacement
  )
  
  # Create a new dataframe with the original data and the calculated new position
  new_gps_df <- rbind(current_gps_df, c(new_coords[2], new_coords[1], NA, NA))
  
  return(new_gps_df)
}

icon.start <- makeAwesomeIcon(
  icon = "flag", markerColor = "green",
  library = "fa",
  iconColor = "black"
)

icon.end <- makeAwesomeIcon(
  icon = "flag", markerColor = "red",
  library = "fa",
  iconColor = "black"
)

icon.latest <- makeAwesomeIcon(
  icon = "flag", markerColor = "purple",
  library = "fa",
  iconColor = "black"
)

latitude <- as.numeric(c("27.7938", "27.8287", "27.86", "27.8253"))
longitude <- as.numeric(c("-84.2426", "-84.2542", "-84.1608", "-84.1542"))
rhombus <- data.frame(latitude, longitude)

if ("Windows" == Sys.info()['sysname']){
conn <- serialConnection("arduino", port="com4", mode="4800,n,8,1")
} else {
  conn <- serialConnection("arduino", port="ttyUSB0", mode="4800,n,8,1")
}
open(conn)
#while(TRUE) { print(read.serialConnection(conn) ) }
#close(conn)

# "$IIGGA,192556.0,2740.5894,N,08318.1806,W,2,9,1.1,9.629,M,-25.062,M,4.0,*63$IIGLL,2740.5894,N,08318.1806,W,192556.0,A,A*5C$IIHDT,276.0,T*21$IIVTG,278.7,T,340.5,M,9.14,N,16.93,K,A*0D$IIZDA,192556.05,02,09,2023,00,00*70$IIGGA,192557.0,2740.5897,N,08318.1835,W,2,9,1.1,9.293,M,-25.061,M,5.0,*66$IIGLL,2740.5897,N,08318.1835,W,192557.0,A,A*5E$IIHDT,276.0,T*21$IIVTG,278.3,T,340.2,M,9.1,N,16.85,K,A*3D$IIZDA,192557.05,02,09,2023,00,00*71$IIGGA,192558.0,2740.5900,N,08318.1862,W,2,9,1.1,9.812,M,-25.061,M,6.0,*64$IIGLL,2740.5900,N,08318.1862,W,192558.0,A,A*5C$IIHDT,276.0,T*21$IIVTG,278.4,T,340.2,M,9.04,N,16.75,K,A*00$IIZDA,192558.05,02,09,2023,00,00*7E$IIGGA,192559.0,2740.5904,N,08318.1890,W,2,9,1.1,10.179,M,-25.061,M,7.0,*51$IIGLL,2740.5904,N,08318.1890,W,192559.0,A,A*54$IIHDT,277.0,T*20$IIVTG,278.4,T,340.3,M,9.11,N,16.87,K,A*08$IIZDA,192559.05,02,09,2023,00,00*7F$IIGGA,192600.0,2740.5908,N,08318.1920,W,2,9,1.1,9.662,M,-25.061,M,7.0,*6D$IIGLL,2740.5908,N,08318.1920,W,192600.0,A,A*5D$IIHDT,277.0,T*20$IIVTG,278.3,T,340.1,M,9.22,N,17.07,K,A*04$IIZDA,192600.05,02,09,2023,00,00*70$IIGGA,192601.0,2740.5911,N,08318.1950,W,2,9,1.1,9.236,M,-25.061,M,7.0,*66$IIGLL,2740.5911,N,08318.1950,W,192601.0,A,A*53$IIHDT,277.0,T*20$IIVTG,278.1,T,339.9,M,9.18,N,17.0,K,A*3E$IIZDA,192601.05,02,09,2023,00,00*71$IIGGA,192602.0,2740.5915,N,08318.1976,W,2,9,1.1,9.76,M,-25.061,M,4.0,*50$IIGLL,2740.5915,N,08318.1976,W,192602.0,A,A*50$IIHDT,277.0,T*20$IIVTG,278.3,T,340.1,M,9.09,N,16.83,K,A*00$IIZDA,192602.05,02,09,2023,00,00*72"


# Define server logic required to draw a histogram


# Run the application 
