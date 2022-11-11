library(openair)
library(plotrix)

source("~/Downloads/Dermet/R/polarWind.R", echo=TRUE)
source("~/Downloads/Dermet/R/archimedeanspiral.R", echo=TRUE)


x <- Sys.time()

# Consistent NE wind

df <- data.frame(
  date = seq(0, 60*60*24, 60*60) + x,
  ws = runif(25,0,20),
  wd = runif(25, 40, 90),
  p = runif(25)
)


plot(x = seq(1:25), y = df$wd, type = "l", ylim = c(0,360), lwd = 3)
polar.plot(lengths = seq(1:25), polar.pos = df$wd, rp.type = "p", lwd = 3)
polarWind(df, type = "l", cols = "black", offset = 0)
polarWind(df, type = "l", cols = "black")
polarWind(df, type = "p", pollutant = "p")
plot(x = seq(1:25), y = seq(0,360,360/24), type = "l", ylim = c(0,360))
polar.plot(lengths = seq(1:25), polar.pos = seq(0,360,360/24), rp.type = "p")



#  Low wind movement
df <- data.frame(
  date = seq(0, 60*60*24, 60*60) + x,
  ws = runif(25,0,3),
  wd = runif(25, 0, 360),
  p = runif(25)
)

anti_df <- df[25:1,]
anti_df$date <- df$date


plot(x = seq(1:25), y = df$wd, type = "l", ylim = c(0,360))
polar.plot(lengths = seq(1:25), polar.pos = df$wd, rp.type = "p")
polarWind(df, type="l", pollutant="ws", cols = "black", offset = 0)
polarWind(anti_df, type="l", pollutant="ws", cols = "black", offset = 0)
polarWind(df, type="l", pollutant="ws", offset = 0)
polarWind(df, pollutant="ws", offset = 0)
polarWind(df, type = "l", pollutant="ws", cols = "black")
polarWind(anti_df, type = "l", pollutant="ws", cols = "black")

polarWind(df, type="l", pollutant="")
polarWind(df, type="l", pollutant="ws")
polarWind(df, type="l", pollutant="ws", cols = "black")



## https://opendata.bristol.gov.uk/explore/dataset/met-data-bristol-lulsgate/table/?sort=date_time
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(tibbletime)

wd <- read_delim("met-data-bristol-lulsgate.csv", delim = ";")

colnames(wd) <- c(
  "date",
  "temp",
  "dp_temp",
  "ws",
  "wd",
  "atm",
  "vis",
  "wct8",
  "rel_hum",
  "wct10"
)

wd_mon <- wd[nrow(wd):1,] %>%
  as_tbl_time(index = date) %>%
  filter_time(
    ~"2022-02-18"
  ) %>%
  as.data.frame()

polarWind(wd_mon, type = "p", lwd = 4, pollutant = "ws", hours = 23.5)
# ~"2022-11-03" = low_wind
# ~"2022-11-06" = low_wind then speeding up in afternoon
# ~"2022-11-08" = constant
# ~"2022-02-18" = Feb gales

#wd_mon$ws <- wd_mon$ws + runif(48) - 0.5


polarWind(wd_mon, hours = 23.5, type = "p", cols = "black")
polarWind(wd_mon, hours = 23.5, type = "l", cols = "black")
polarWind(wd_mon, hours = 23.5, type = "l")



