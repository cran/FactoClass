\name{stableclus}
\alias{stableclus}
\title{Stable Clusters from K-means}
\description{It performs stable clusters from several partitions from K-means changing the initial points. It uses the coordinates of an previous principal axes method
}
\usage{
stableclus(dudi,part=2,k.clust=2,ff.clus=NULL,bplot=TRUE,kmns=FALSE)
}
\arguments{
 \item{dudi}{ object of type \code{dudi}}
 \item{part}{the number of partitions to obtain}
 \item{k.clust}{the number of clusters each partition}
 \item{ff.clus}{the number of ended clusters, if is NULL (default), it asks in the console the values \code{ff.clus}}
 \item{bplot}{if is TRUE, the barplot of the stable clusters is ploted}
 \item{kmns}{when \code{hmns}=TRUE, a \code{kmeansW} is performed with argument \code{centers} equal to the centroids of the stable classes retained}
}
\details{
Lebart, L. and Morineau, A. and Piron, M. (1995) Statisitique exploratoire
multidimensionnelle, Paris.
}
\value{
 list with: 
 \item{class}{vector indicating the cluster of each element}
 \item{stacl}{vector containing the stable clusters in descending order}
}

\author{
 Carlos Andres Arias \email{caariasr@unal.edu.co>},
 Campo Elias Pardo \email{cepardot@unal.edu.co},
 Diana Carolina Zarate \email{dczarated@unal.edu.co}
}
\examples{
 data(BreedsDogs)
 acm <- dudi.acm(BreedsDogs[,-7],scannf = FALSE,nf=10)
 stableclus(acm,2,3,4)
}
\keyword{multivariate}
