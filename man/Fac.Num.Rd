\name{Fac.Num}
\alias{Fac.Num}
\title{Division of qualitative and quantitative variables}
\description{
An object of class data.frame is divided into a list with two tables, one with quantitative variables
and the other with qualitative variables.
}
\usage{
Fac.Num(tabla)
}
\arguments{
  \item{tabla}{  object of class 'data.frame'}
}

\value{
 It returns one list with one or two objects of class data.frame with the following characteristics:
  \item{factor}{  table with the  qualitative variables }
  \item{numeric}{  table with the quantitative  variables }
}

\author{ Pedro Cesar Del Campo \email{pcdelcampon@unal.edu.co}}

\examples{

   data(DogBreeds)
   Fac.Num(DogBreeds)

   data(iris)
   Fac.Num(iris)

}
\keyword{multivariate}
