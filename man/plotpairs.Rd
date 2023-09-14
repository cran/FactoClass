\name{plotpairs}
\alias{plotpairs}
\title{Modified pairs plot}
\description{
 Modified pairs plot: marginal kernel densities in diagonal, bivariated kernel densities in triangular superior; and scatter bivariate plots in triangular inferior
}
\usage{
plotpairs(X,maxg=5,cex=1)
}
\arguments{
  \item{X}{matrix or data.frame of numeric colums}
  \item{maxg}{maximum number of variables to plot}
  \item{cex}{size of the points in dispersion diagrams}
}
\details{
  Plot row profiles in horizontal form and columns profiles in vertical form 
 }
\author{Campo Elias Pardo \email{cepardot@unal.edu.co} 
}
\value{The function does not return values
} 
\examples{
data(iris)
plotpairs(iris[,-5])
}
\keyword{hplot}

