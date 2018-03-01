\name{cluster.carac}
\alias{cluster.carac}
\title{Cluster Characterization by Variables}
\description{
It makes the characterization of the classes or cluster considering the variables in tabla. 
These variables can be
quantitative, qualitative or frequencies.
}
\usage{
cluster.carac( tabla,class,tipo.v="d",v.lim= 2,dn=3,dm=3,neg=TRUE)
}
\arguments{
  \item{tabla}{object data.frame with variables of characterization, the variables must be
               of a single type (quantitative, qualitative or frequencies)}
  \item{class}{vector that determines the partition of the table}
  \item{tipo.v}{type of variables: quantitative("continuas"), qualitative ("nominales") or frequencies("frecuencia")  }
  \item{v.lim}{test value to show the variable or category like characteristic.}
   \item{dn}{number of decimal digits for the p and test values.}
    \item{dm}{number of decimal digits for the means.}
\item{neg}{if neg=TRUE, the variables or categories with negative test values are showed.}
}
\details{
 For nominal or frecuency variables it compares the percentage of the categories within 
 each class with the global percentage.
 For continuous variables it compares the average within each class with the general average.
 Categories and variables are ordered within each class by the test values and it shows only 
 those that pass the threshold v.lim.
}
\value{
 Object of class list. It has the characterization of each class or cluster.
}

\author{ Pedro Cesar del Campo \email{pcdelcampon@unal.edu.co}, Campo Elias Pardo \email{cepardot@unal.edu.co},
         Mauricio Sadinle \email{msadinleg@unal.edu.co}}

\references{
Lebart, L. and Morineau, A. and Piron, M. (1995) Statisitique exploratoire
multidimensionnelle, Paris.
}
\examples{

data(DogBreeds)
DB.act <- DogBreeds[-7]  # active variables
DB.function <- subset(DogBreeds,select=7)   
cluster.carac(DB.act,DB.function,"ca",2.0)  #  nominal variables


data(iris)
iris.act <- Fac.Num(iris)$numeric
class <- Fac.Num(iris)$factor
cluster.carac(iris.act,class,"co",2.0)  #  continuous variables

# frequency variables
data(DogBreeds)
attach(DogBreeds)
weig<-table(FUNC,WEIG)
weig<-data.frame(weig[,1],weig[,2],weig[,3])
cluster.carac(weig,  row.names(weig), "fr", 2) # frequency variables
detach(DogBreeds)
}

\keyword{multivariate}
\keyword{hplot}
