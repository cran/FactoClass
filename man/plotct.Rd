\name{plotct}
\alias{plotct}
\title{Row and Column Profiles of a Contingency Table}
\description{
 It plots barplot profiles of rows or columns from a contingency table including marginal profiles
}
\usage{
plotct(x,profiles="both",legend.text=TRUE,tables=FALSE,nd=1,... )
}
\arguments{
  \item{x}{contingency table}
  \item{profiles}{select profiles: "both" file and column profiles in two graph devices, "row" only row profiles, "col" only column profiles}
  \item{legend.text}{if it is TRUE a box with legends is included at the right}
  \item{tables}{logical, if TRUE tables with marginals are returned}
  \item{nd}{number of decimals to profiles as percentages}
  \item{\dots}{further arguments passed to or from other methods}
}
\details{
  Plot row profiles in horizontal form and columns profiles in vertical form 
 }
\author{Camilo Jose Torres \email{cjtorresj@unal.edu.co} , Campo Elias Pardo \email{cepardot@unal.edu.co} 
}
\value{
 if tables=TRUE, object of class \code{list} with the following:
 
 \item{ct}{contingengy table with row and column marginals}
 \item{perR}{row profile with marginal, in percent}
 \item{perC}{column profile with marginal, in percent}
} 
\examples{
mycolors<-colors()[c(1,26,32,37,52,57,68,73,74,81,82,84,88,100)]
data(Bogota)
plotct(Bogota[,2:7],col=mycolors)
# return tables with marginals
tabs <- plotct(Bogota[,2:7],col=mycolors,tables=TRUE,nd=0)
}
\keyword{hplot}

