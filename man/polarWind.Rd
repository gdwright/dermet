\name{polarWind}
\alias{polarWind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
polarWind(mydata, pollutant = "ws", type = "p", start = "", grid.line = 6, hours = 24, offset = "", key.title = "", breaks = 10, time.angle = 0, block.colour = "blue", line.col = "powderblue", pch = 20, cex = 1, lwd = 2, key.position = "right", key = TRUE, key.header = pollutant, cols = "default")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
%%     ~~Describe \code{mydata} here~~
}
  \item{pollutant}{
%%     ~~Describe \code{pollutant} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{grid.line}{
%%     ~~Describe \code{grid.line} here~~
}
  \item{hours}{
%%     ~~Describe \code{hours} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
}
  \item{key.title}{
%%     ~~Describe \code{key.title} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{time.angle}{
%%     ~~Describe \code{time.angle} here~~
}
  \item{block.colour}{
%%     ~~Describe \code{block.colour} here~~
}
  \item{line.col}{
%%     ~~Describe \code{line.col} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{key.position}{
%%     ~~Describe \code{key.position} here~~
}
  \item{key}{
%%     ~~Describe \code{key} here~~
}
  \item{key.header}{
%%     ~~Describe \code{key.header} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mydata, pollutant = "ws", type = "p", start = "", grid.line = 6, 
    hours = 24, offset = "", key.title = "", breaks = 10, time.angle = 0, 
    block.colour = "blue", line.col = "powderblue", pch = 20, 
    cex = 1, lwd = 2, key.position = "right", key = TRUE, key.header = pollutant, 
    cols = "default") 
{
    if (!any(sapply(mydata, inherits, what = "POSIXct"))) {
        stop("No date column in POSIXct class found")
    }
    dateCol <- names(which(sapply(mydata, inherits, what = "POSIXct")))
    vars <- c(dateCol, "wd")
    if (pollutant != "") {
        vars <- c(vars, pollutant)
    }
    if (is.na(as.numeric(start))) {
        startTime <- mydata[1, dateCol]
    }
    else if (inherits(start, what = "POSIXct")) {
        startTime <- start
    }
    else {
        stop("Start is not in class POSIXct")
    }
    seconds <- hours * 60 * 60
    startTime <- as.POSIXct(startTime)
    endTime <- startTime + seconds
    if (any(mydata[, dateCol] == startTime)) {
        firstCell <- which(mydata[, dateCol] == startTime)
    }
    else {
        stop("Start time not present in date col")
    }
    if (any(mydata[, dateCol] == endTime)) {
        lastCell <- which(mydata[, dateCol] == endTime)
    }
    else {
        stop("End time not present in data col")
    }
    numCells <- 1 + lastCell - firstCell
    if (!numCells > 1) {
        stop("Not enought data points to plot graph")
    }
    if (offset == "") {
        offset <- numCells
    }
    else if (offset < numCells) {
        warning("A smaller offset can distort data representation")
    }
    plotSize <- 1.15 * (offset + numCells)
    totalCells <- seq(firstCell, lastCell, by = 1)
    subdata <- data.frame(mydata[totalCells, vars])
    first <- as.numeric(mydata$date[firstCell])
    last <- as.numeric(mydata$date[lastCell])
    if (any(vars == pollutant)) {
        mn <- min(subdata[, pollutant], na.rm = TRUE)
        mx <- max(subdata[, pollutant], na.rm = TRUE)
        levs <- pretty(mn:mx, n = breaks)
        nlev <- length(levs)
        COLS <- openColours(cols, nlev - 1)
        subdata$Int <- findInterval(subdata[, pollutant], levs)
        subdata$col <- COLS[subdata$Int]
        subdata$col[which(is.na(subdata$col))] <- "grey"
    }
    else {
        subdata$col <- block.colour
    }
    subdata$R <- offset + (as.numeric(subdata$date) - first)/((last - 
        first)/(numCells - 1))
    subdata$rads <- (subdata$wd * 2 * pi)/360
    subdata$x <- subdata$R * sin(subdata$rads)
    subdata$y <- subdata$R * cos(subdata$rads)
    GL <- ((numCells - 1) * grid.line)/hours
    innerRing <- offset
    outerRing <- offset + (hours/grid.line) * GL
    outerRingPlus <- outerRing * 1.1
    time.theta <- time.angle * (2 * pi)/(360)
    plot(-plotSize:plotSize, -plotSize:plotSize, type = "n", 
        asp = 1, bty = "n", ann = FALSE, xaxt = "n", yaxt = "n")
    text(0, outerRingPlus, "N", cex = 0.7)
    text(outerRingPlus, 0, "E", cex = 0.7)
    text(-outerRingPlus, 0, "W", cex = 0.7)
    text(0, -outerRingPlus, "S", cex = 0.7)
    lines(c(0, 0), c(outerRing, innerRing), col = "grey")
    lines(c(outerRing, innerRing), c(0, 0), col = "grey")
    lines(-c(outerRing, innerRing), c(0, 0), col = "grey")
    lines(c(0, 0), -c(outerRing, innerRing), col = "grey")
    draw.circle(0, 0, innerRing, border = "grey")
    text(innerRing * sin(time.theta), innerRing * cos(time.theta), 
        format(subdata$date[1], "\%H:\%M"), adj = c(0, 0), cex = 0.7, 
        col = "red")
    for (i in 1:trunc(hours/grid.line)) {
        draw.circle(0, 0, innerRing + i * GL, border = "grey")
        text((innerRing + i * GL) * sin(time.theta), (innerRing + 
            i * GL) * cos(time.theta), format(startTime + i * 
            grid.line * 3600, "\%H:\%M"), col = "red", cex = 0.7, 
            adj = c(0, 0))
    }
    if (key.title == "") {
        KT <- pollutant
    }
    else {
        KT <- key.title
    }
    if (type == "p") {
        points(subdata$x, subdata$y, pch = pch, col = subdata$col, 
            cex = cex)
    }
    else if (type == "l") {
        lines <- data.frame(subdata$x, subdata$y, subdata$col)
        lines <- na.omit(lines)
        for (i in 1:(length(lines$subdata.x) - 1)) {
            archimedean.spiral(c(lines$subdata.x[i], lines$subdata.y[i]), 
                c(lines$subdata.x[i + 1], lines$subdata.y[i + 
                  1]), colour = paste(lines$subdata.col[i]), 
                lwd = lwd)
        }
    }
    else if (type == "b") {
        lines <- data.frame(subdata$x, subdata$y, subdata$col)
        lines <- na.omit(lines)
        for (i in 1:(length(lines$subdata.x) - 1)) {
            archimedean.spiral(c(lines$subdata.x[i], lines$subdata.y[i]), 
                c(lines$subdata.x[i + 1], lines$subdata.y[i + 
                  1]), colour = line.col, lwd = lwd)
        }
        points(subdata$x, subdata$y, pch = pch, col = subdata$col, 
            cex = cex)
    }
    else {
        stop("Type must be p, l or b")
    }
    if (!pollutant == "") {
        gradient.rect(outerRing * 1.25, -outerRingPlus, outerRing * 
            1.3, outerRingPlus, col = COLS, nslices = nlev, gradient = "y")
        text(outerRing * 1.275, outerRingPlus * 1.1, KT, cex = 0.7)
        text(rep(outerRing * 1.3, nlev), seq(-outerRingPlus, 
            outerRingPlus, by = 2 * outerRingPlus/(nlev - 1)), 
            paste("-", levs, sep = " "), cex = 0.7, adj = c(0, 
                0.5))
    }
    text(-outerRingPlus, -outerRingPlus, paste(format(startTime, 
        "\%d/\%m/\%Y \%H:\%M"), "- Start", sep = " "), cex = 0.7, 
        adj = c(0, 0))
    text(-outerRingPlus, -outerRingPlus - 50, paste(format(endTime, 
        "\%d/\%m/\%Y \%H:\%M"), "- End", sep = " "), cex = 0.7, adj = c(0, 
        0))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
