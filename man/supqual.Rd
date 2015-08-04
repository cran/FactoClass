\encoding{latin1}
\name{supqual}
\alias{supqual}
\title{Projection of Qualitative Variables in PCA and MCA}
\description{
 It returns the coordinates and aids to the interpretation when one or more qualitative variables are projected as ilustrative in  PCA or MCA
}
\usage{
supqual(du,qual)
}
\arguments{
  \item{du}{a object of class ``pca'' or ``acm'' (``dudi'') obtained with \code{dudi.pca} or \code{dudi.acm} of package \code{ade4}}
  \item{qual}{a data.frame of qualitative variables as factors}
}

\author{Campo Elias Pardo \email{cepardot@unal.edu.co} 
}
\value{object of class \code{list} with the following:
 
 \item{wcat}{weight of the categories in PCA case}
  \item{ncat}{frequency of the categories in MCA case}
 \item{dis2}{square distance to the origin from the complete space}
 \item{coor}{factorial coordinates}
 \item{tv}{test values}
 \item{cos2}{square cosinus}
 \item{scr}{relation of correaltion}
} 
\examples{
# in PCA
data(admi)
Y<-admi[,2:6]
pcaY<-dudi.pca(Y,scannf=FALSE)
Yqual<-admi[,c(1,8)]
supqual(pcaY,Yqual)
# in MCA
Y<-admi[,c(8,11,9,10)]
mcaY<-dudi.acm(Y,scannf=FALSE)
supqual(mcaY,admi[,c(1,13)])
}
\keyword{hplot}

