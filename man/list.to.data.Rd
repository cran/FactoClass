\name{list.to.data}
\alias{list.to.data}
\title{list to data.frame}
\description{Modification of an object of class \code{list} into an object of class \code{data.frame}.}
\usage{
list.to.data(lista,nvar="clasif")
}
\arguments{
  \item{lista}{\code{list} that contains several \code{data.frame} of the same structure.}
  \item{nvar}{(Optional) Name of the new variable that considers the partition 
              given by the elements of the list.  }
}
\details{
 This function turns an object of class \code{list} into an object of class \code{data.frame}, this function is 
 used internally to create objects of class data.frame to make tables in \emph{LaTeX} format.
}

\value{
 Object of class data.frame.
}
\author{ Pedro Cesar Del Campo \email{pcdelcampon@unal.edu.co}}

\examples{

 A <- data.frame(r1=rnorm(5),r2=rnorm(5))
 B <- data.frame(r1=rnorm(15),r2=rnorm(15))

 LL <- list(A=A,B=B)
 LL
 list.to.data(LL)


}
\keyword{multivariate}
