\name{kmeansW}
\alias{kmeansW}
\alias{kmeansW}
\title{K-means with Weights of the Elements}
\description{It is a modification of kmeans Hartigan-Wong algorithm to consider the weight of the elements 
to classify.}
\usage{
kmeansW(x, centers, weight = rep(1,nrow(x)),
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
 diminishing inertia intra classes. In this version the Fortran code kmnsW.f was changed by C++ code kmeanw.cc programed by Camilo Jose Torres, modifing C code programed by Burkardt.
}
\value{
 object of class \code{FactoClass.tex} with the following characteristics: 
 \item{cluster}{vector indicating the cluster of each element.}
 \item{...    }{}
}
\references{  Hartigan, J. A. and Wong, M. A. (1979).
  A K-means clustering algorithm.
  \emph{Applied Statistics} \bold{28}, 100--108.

Burkardt, J. (2008).
ASA136 The K-Means Algorithm.
 \url{http://people.sc.fsu.edu/~burkardt/cpp_src/asa136/asa136.html}
}
\author{
Camilo José Torres \email{cjtorresj@unal.edu.co},  Campo Elias Pardo \email{cepardot@unal.edu.co}, Pedro Cesar del Campo \email{pcdelcampon@unal.edu.co}
}

\examples{
 data(Bogota)
 ac.bog <- Bogota[-1]
 il.bog <- Bogota[ 1]
 
 acs <- dudi.coa( ac.bog, nf=6 , scannf = FALSE )
 
 kmeansW( acs$li, 7, acs$lw )
 
}
\keyword{multivariate}
\keyword{cluster}
