\name{kmeansW}
\alias{kmeansW}
\alias{kmeansW}
\title{K-means with Weights of the Elements}
\description{It is a modification of kmeans Hartigan-Wong algorithm to consider the weight of the elements 
to classify.}
\usage{
kmeansW(x, centers, weight = rep(1/nrow(x),nrow(x)),
        iter.max = 10, nstart = 1)
}
\arguments{
 \item{x       }{A numeric vector, matrix or data frame.}
 \item{centers }{Either the number of clusters or a set of initial (distinct) cluster centres. 
                 If a number, a random set of (distinct) rows in x is chosen as the initial centres.}
 \item{weight  }{weight of the elements of x. by default the same.}
 \item{iter.max}{The maximum number of iterations allowed.}
 \item{nstart  }{If centers is a number, how many random sets should be chosen?}
}
\details{
 With the 'Hartigan-Wong' algorithm, this function performs the \emph{K-means} clustering
 diminishing inertia intra classes.
 
}
\value{
 object of class \code{FactoClass.tex} with the following characteristics: 
 \item{cluster}{vector indicating the cluster of each element.}
 \item{...    }{}
}

\author{
 Pedro Cesar del Campo \email{pcdelcampon@unal.edu.co},
 Campo Elias Pardo \email{cepardot@unal.edu.co},
 Mauricio Sadinle \email{msadinleg@unal.edu.co}
       \url{http://www.docentes.unal.edu.co/cepardot}}

\examples{
 data(Bogota)
 ac.bog <- Bogota[-1]
 il.bog <- Bogota[ 1]
 
 acs <- dudi.coa( ac.bog, nf=6 , scannf = FALSE )
 
 kmeansW( acs$li, 7, acs$lw )
 
}
\keyword{multivariate}
