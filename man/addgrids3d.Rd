% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addgrids3d.R
\name{addgrids3d}
\alias{addgrids3d}
\title{Add grids to a scatterplot3d (modified)}
\usage{
addgrids3d(x, y = NULL, z = NULL, grid = TRUE, col.grid = "grey",
  lty.grid = par("lty"), lab = par("lab"), lab.z = mean(lab[1:2]),
  scale.y = 1, angle = 40, xlim = NULL, ylim = NULL, zlim = NULL)
}
\arguments{
\item{x, y, z}{numeric vectors specifying the x, y, z coordinates of points.
x can be a matrix or a data frame containing 3 columns corresponding to
the x, y and z coordinates. In this case the arguments y and z are optional}

\item{grid}{specifies the facet(s) of the plot on which grids should be drawn.
Possible values are the combination of "xy", "xz" or "yz".
Example: grid = c("xy", "yz"). The default value is TRUE to add grids only on xy facet.}

\item{col.grid, lty.grid}{color and line type to be used for grids}

\item{lab}{a numerical vector of the form c(x, y, len).
The values of x and y give the (approximate) number of tickmarks on the x and y axes.}

\item{lab.z}{the same as lab, but for z axis}

\item{scale.y}{of y axis related to x- and z axis}

\item{angle}{angle between x and y axis}
\item{xlim}{the x limits (min, max) of the plot}
\item{ylim}{the y limits (min, max) of the plot}
\item{zlim}{the z limits (min, max) of the plot.}
}
\description{
The goal of this function is to add grids on an existing
 plot created using the package scatterplot3d
}
\note{
Users who want to extend an existing scatterplot3d graphic with the
 function addgrids3d, should consider to set the arguments scale.y, angle, ...,
 to the value used in scatterplot3d.
}
\examples{
library(scatterplot3d)
data(iris)
scatterplot3d(iris[, 1:3], pch = 16, grid=TRUE, box=FALSE)
addgrids3d(iris[, 1:3], grid = c("xy", "xz", "yz"))
}
\references{
http://www.sthda.com
}
\author{
Alboukadel Kassambara \email{alboukadel.kassambara@gmail.com}
}
