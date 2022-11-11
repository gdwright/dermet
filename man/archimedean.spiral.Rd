\name{archimedean.spiral}
\alias{archimedean.spiral}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
archimedean.spiral(x, y, colour = "blue", lwd = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{colour}{
%%     ~~Describe \code{colour} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
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
function (x, y, colour = "blue", lwd = 2) 
{
    Rx <- ((x[1]^2) + (x[2]^2))^0.5
    Ry <- ((y[1]^2) + (y[2]^2))^0.5
    if (Rx <= Ry) {
        Ra <- Rx
        Rb <- Ry
        a <- x
        b <- y
    }
    else if (Ry < Rx) {
        Ra <- Ry
        Rb <- Rx
        a <- y
        b <- x
    }
    if (Ra != 0 & Rb != 0) {
        thetaA <- acos(a[1]/Ra)
        thetaB <- acos(b[1]/Rb)
    }
    else if (Rb == 0) {
        thetaA <- acos(a[1]/Ra)
        thetaB <- thetaA
    }
    else if (Ra == 0) {
        thetaB <- acos(b[1]/Rb)
        thetaA <- thetaB
    }
    if (a[2] < 0) {
        thetaA <- 2 * pi - thetaA
    }
    if (b[2] < 0) {
        thetaB <- 2 * pi - thetaB
    }
    if (thetaB < thetaA) {
        thetaA <- -2 * pi + thetaA
        thetaB <- -2 * pi + thetaB
    }
    if (abs(thetaA - thetaB) > pi) {
        if (thetaA >= 0) {
            thetaA <- (-2) * pi + thetaA
            thetaB <- (-2) * pi - (2 * pi - thetaB)
        }
        else {
            thetaB <- (-2) * pi + thetaB
            thetaA <- (-2) * pi - (2 * pi - thetaA)
        }
    }
    if (thetaA == thetaB) {
        lines(c(x[1], y[1]), c(x[2], y[2]), col = col, lwd = lwd)
    }
    else {
        M <- (Ra - Rb)/(thetaA - thetaB)
        N <- Ra - M * thetaA
        Res <- round(abs(thetaA - thetaB) * (180/pi), digits = 0)
        if (Res < 2) {
            Res <- 2
        }
        data <- matrix(, Res, 2)
        data[, 1] <- seq(Ra, Rb, length.out = Res)
        if (M != 0) {
            data[, 2] <- (data[, 1] - N)/M
        }
        else if (M == 0) {
            data[, 2] <- seq(min(thetaA, thetaB), max(thetaA, 
                thetaB), length.out = Res)
        }
        dataXY <- matrix(, Res, 2)
        dataXY[, 1] <- data[, 1] * cos(data[, 2])
        dataXY[, 2] <- data[, 1] * sin(data[, 2])
        lines(dataXY[, 1], dataXY[, 2], col = colour, asp = 1, 
            lwd = lwd)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
