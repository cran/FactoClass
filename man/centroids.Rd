\name{centroids}
\alias{centroids}
\title{Centroids of the Classes of a Partition}
\description{It evaluates the centroids of a partition with the weights in \code{rw}
}
\usage{centroids(df,cl,rw=rep(1/nrow(df),nrow(df)))
}
\arguments{
  \item{df}{object of class \code{data.frame}, with the data of variables or coordinates}
  \item{cl}{vector indicating the cluster of each element}
  \item{rw}{weight of the rows of df, by default the same}
}
\value{
Object of class \code{list} with the following:
\item{centroids}{class centroids}
\item{weights}{class weights} 
\item{cr}{correlation ratios} 
}
\author{Campo Elias Pardo \email{cepardot@unal.edu.co}
}

\examples{
data(iris)
centroids(iris[,-5],iris[,5])
}
\keyword{multivariate}
