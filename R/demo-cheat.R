df <- read_csv("sample-wind-plot.csv")
poly_plot <- read_csv("sample-poly-plot.csv")

df$date <- seq(0, 60*60*24, 60*60*24/92) + Sys.time()

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
legend(5,360, c("ws", "wd"), col = c("blue", "red"), lty = 1, lwd = 3)
axis(side = 4, at = c(0,90, 180, 270, 360))      # Add second axis
mtext("wd", side = 4, line = 3)             # Add second axis label

plot(x = seq(1,93, 1), y = df$wd, type = "l")

View(poly_plot)

polar.plot(
  poly_plot$score,
  polar.pos = poly_plot$dir,
  lwd = 3,
  rp.type = "p",
  radial.lim = c(0,10)
)


#constant NE wind
# df <- data.frame(
#   date = seq(0, 60*60*24, 60*60) + x,
#   ws = runif(25,0,20),
#   wd = runif(25, 40, 90),
#   p = runif(25)
# )

polar.plot(
  seq(1,93,1),
  polar.pos = df$wd,
  rp.type = "p",
  lwd = 3
)

polar.plot(
  seq(1,93,1),
  polar.pos = df$wd,
  rp.type = "p",
  lwd = 3,
  start = 90,
  clockwise = TRUE
)

polar.plot(
  seq(1,93,1),
  polar.pos = df$wd,
  rp.type = "p",
  lwd = 3,
  start = 90,
  clockwise = TRUE
)

# 4 wind types

spiral <- seq(0,360, 360/92)
sea_breeze <- c(seq(0,180, 180/45), 180, seq(180, 0, -180/45))
const_wind <- rep(0, 93)
low_wind <- runif(93, 0, 360)

polar.plot(
  seq(1,93,1),
  polar.pos = -sea_breeze,
  rp.type = "p",
  lwd = 3,
  start = 90,
  clockwise = T
)


radius <- 1:100
angle <- 10 * radius

polar.plot(
  radius,
  polar.pos = angle,
  rp.type = "p",
  lwd = 3
)

spiral24 <- seq(0,360, 360/24)
sea_breeze24 <- c(seq(0,180, 180/11), 190, seq(180, 0, -180/11))
const_wind24 <- rep(0, 25)
low_wind24 <- runif(25, 0, 360)


df_polar <- data.frame(
  date = seq(0, 60*60*24, 60*60) + Sys.time(),
  wd = spiral24,
  ws = sea_breeze24
)


polarWind(df_polar, type = "l", lwd = 4)

polarWind(df %>% as.data.frame(), type = "l")
