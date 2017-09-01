\name{stableclus}
\alias{stableclus}
\title{Stable clusters for cluster analysis}
\description{Performs Stable Cluster Algorithm for cluster analysis, using factorial coordinates from a \code{dudi} object}

\usage{
stableclus(dudi,part,k.clust,ff.clus=NULL,bplot=TRUE,kmns=FALSE) 
}
\arguments{
\item{dudi    }{A \code{dudi} object, result of a previous factorial analysis using \code{ade4} or \code{FactoClass}}
 \item{part    }{Number of partitions}
 \item{k.clust }{Number of clusters in each partition}
 \item{ff.clus }{Number of clusters for the final output, if NULL it asks in the console (Default NULL)}
 \item{bplot   }{if TRUE, prints frequencies barplot of each cluster in the product partition (Default TRUE)}
 \item{kmns    }{if TRUE, the process of consolidating the classification is performed (Default FALSE)}
}
\details{
 Diday (1972) (cited by Lebart et al. (2006)) presented a method for cluster analysis in an attempt to solve one of the inconvinients with the \emph{kmeans} 
 algorithm, which is convergence to local optims. Stable clusters are built by performing different partitions (using \code{kmeansW} algorithmn), each one with different initial points. The groups are then formed by selecting the individuals belonging to the same cluster in every partion. }
\value{
object of class \code{stableclus} with the following characteristics: 
 \item{cluster}{vector indicating the cluster of each element.}
 \item{...    }{}
}
\references{ 
Arias, C. A.; Zarate, D.C. and Pardo C.E. (2009), 'Implementacion del metodo de grupos estables en el paquete FactoClass de R', in: XIX Simposio Colombiano de Estadistica.
Estadisticas Oficiales
Medell?n Colombia, Julio 16 al 20 de 2009
Universidad Nacional de Colombia. Bogota.
\url{http://www.docentes.unal.edu.co/cepardot/docs/SimposiosEstadistica/AriasZaratePardo09.pdf}

Lebart, L. (2015), 'DtmVic: Data and Text Mining - Visualization, Inference, Classification. Exploratory statistical processing of complex data sets comprising both numerical and textual data.', Web.
 \url{http://www.dtmvic.com/}

Lebart, L., Morineau, A., Lambert, T. and Pleuvret, P. (1999), \emph{SPAD}. Syst?me Pour L'Analyse des Don?es,
Paris.

Lebart, L., Piron, M. and Morineau, A. (2006), Statisitique exploratoire multidimensionnelle. Visualisation
et inf?rence en fouilles de donn?es, 4 edn, Dunod, Paris.
}
\author{
Carlos Andres Arias \email{caariasr@unal.edu.co},
Campo Elias Pardo \email{cepardot@unal.edu.co}
}


\examples{
 data(ColorAdjective)
 FCcol <-FactoClass(ColorAdjective, dudi.coa,nf=6,nfcl=10,k.clust=7,scanFC = FALSE)
 acs <- FCcol$dudi
# stableclus(acs,3,3,4,TRUE,TRUE)

}
\keyword{multivariate}
\keyword{cluster}
