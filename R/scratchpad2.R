library(openair)
library(plotrix)
library(tidyr)
library(dplyr)
library(magrittr)
library(readr)

source("~/Downloads/Dermet/R/polarWind.R", echo=TRUE)
source("~/Downloads/Dermet/R/archimedeanspiral.R", echo=TRUE)

df <- read_csv("sample-wind-plot.csv")
poly_plot <- read_csv("sample-poly-plot.csv")


par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(x = seq(1,93, 1), y = df$ws, type = "l", col = "blue", lwd = 3, ylab= "ws", xlab = "time")              # Create first plot
par(new = TRUE)                             # Add new plot
plot(
  x = seq(1,93, 1),
  y = df$wd,
  type = "l",
  lwd= 3,
  col = "red",
  axes = FALSE,
  xlab = "",
  ylab = ""
)
axis(side = 4, at = c(0,90, 180, 270, 360))      # Add second axis
mtext("wd", side = 4, line = 3)



?polarAnnulus


polarAnnulus(mydata, pollutant = "pm10")
