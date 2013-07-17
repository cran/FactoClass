\encoding{latin1}
\name{plotct}
\alias{plotct}
\title{Row and Column Profiles of a Contingency Table}
\description{
 It plots barplot profiles of rows or columns from a contingency table including marginal profiles
}
\usage{
plotct(x,profiles="both",legend.text=TRUE,... )
}
\arguments{
  \item{x}{contingency table}
  \item{profiles}{select profiles: "both" file and column profiles in two graph devices, "row" only row profiles, "col" only column profiles}
  \item{legend.text}{if it is TRUE a box with legends is included at the right}
  \item{\dots}{further arguments passed to or from other methods}
}
\details{
  Plot row profiles in horizontal form and columns profiles in vertical form 
 }
\author{Camilo Jose Torres \email{cjtorresj@unal.edu.co} , Campo Elias Pardo \email{cepardot@unal.edu.co} 
  \url{http://www.docentes.unal.edu.co/cepardot}
}
\examples{
mycolors<-colors()[c(1,26,32,37,52,57,68,73,74,81,82,84,88,100)]
data(Bogota)
plotct(Bogota[,2:7],col=mycolors)
}
\keyword{hplot}

