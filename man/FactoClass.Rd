\name{FactoClass}
\alias{FactoClass}
\alias{print.FactoClass}
\alias{analisis.clus}
\title{Combination of Factorial Methods and Cluster Analysis}
\description{
Performs the factorial analysis of the data and a cluster analysis using the \code{nfcl} first factorial
coordinates
}
\usage{
FactoClass( dfact, metodo, dfilu = NULL , nf = 2, nfcl = 10, k.clust = 3, 
            scanFC = TRUE , n.max = 5000 , n.clus = 1000 ,sign = 2.0,
            conso=TRUE , n.indi = 25 )
\method{print}{FactoClass}(x, \dots)
analisis.clus(X,W)
}
\arguments{
 \item{dfact   }{object of class \code{data.frame}, with the data of active variables.}
 \item{metodo  }{function of ade4 for \code{ade4} factorial analysis, \code{dudi.pca},Principal Component Analysis; 
                 \code{dudi.coa}, Correspondence Analysis; \code{witwit.coa}, Internal Correspondence Analysis;
                 \code{dudi.acm}, Multiple Correspondence Analysis ... }
 \item{dfilu   }{ilustrative variables (default NULL)}
 \item{nf      }{number of axes to use into the factorial analysis (default 2)}
 \item{nfcl    }{number of axes to use in the classification (default 10)}
 \item{k.clust }{number of classes to work (default 3)}
 \item{scanFC  }{if is TRUE, it asks in the console the values \code{nf}, \code{nfcl} y \code{k.clust} }
 \item{n.max   }{when \code{rowname(dfact)>=n.max}, k-means is performed previous to hierarchical 
 clustering (default 5000)}
 \item{n.clus  }{when \code{rowname(fact)>=n.max}, the previous k-means is performed with 
 \emph{n.clus} groups (default 1000)}
 \item{sign    }{threshold test value to show the characteristic variables and modalities}
 \item{conso   }{when \code{conso} is TRUE, the process of consolidating the classification is 
 performed (default TRUE)}
 \item{n.indi  }{number of indices to draw in the histogram (default 25)}
 \item{x       }{object of class FactoClass}
 \item{\dots}{further arguments passed to or from other methods}
 \item{X       }{coordinates of the elements of a class}
 \item{W       }{weights of the elements of a class}

}
\details{
Lebart et al. (1995) present a strategy to analyze a data table using multivariate methods, consisting
of an intial factorial analysis according to the nature of the compiled data, followed by the performance
of mixed clustering. The mixed clustering combines hierarchic clustering using the Ward's method with
K-means clustering. Finally a partition of the data set and the characterization of each one of the
classes is obtained, according to the active and illustrative variables, being quantitative, qualitative
or frequency.

FactoClass is a function that connects procedures of the package \code{ade4} to perform the analysis
factorial of the data and from \code{stats} for the cluster analysis.

The function \code{analisis.clus} calculates the geometric characteristics of each class:
size, inertia, weight and square distance to the origin.

For impression in  LaTeX format see \link{FactoClass.tex}

To draw factorial planes with cluster see \link{plotFactoClass}

}
\value{
 object of class \code{FactoClass} with the following:

 \item{dudi      }{object of class \code{dudi} from \code{ade4} with the specifications of the factorial analysis }
 \item{nfcl      }{number of axes selected for the classification }
 \item{k         }{number of classes}
 \item{indices   }{table of indices obtained through WARD method}
 \item{cor.clus  }{coordinates of the clusters}
 \item{clus.summ }{summary of the clusters}
 \item{cluster   }{vector indicating the cluster of each element}
 \item{carac.cate}{cluster characterization by qualitative variables}
 \item{carac.cont}{cluster characterization by quantitative variables}
 \item{carac.frec}{cluster characterization by frequency active variables}
}
\references{
Lebart, L. and Morineau, A. and Piron, M. (1995) Statisitique exploratoire
multidimensionnelle, Paris.
}
\author{
 Pedro Cesar del Campo \email{pcdelcampon@unal.edu.co},
 Campo Elias Pardo \email{cepardot@unal.edu.co}
       \url{http://www.docentes.unal.edu.co/cepardot},
 Ivan Diaz \email{ildiazm@unal.edu.co},       
 Mauricio Sadinle \email{msadinleg@unal.edu.co}
}
\examples{

# Cluster analysis with Correspondence Analysis
data(ColorAdjective)
FC.col <-FactoClass(ColorAdjective, dudi.coa)
6
10
5

FC.col

FC.col$dudi


# Cluster analysis with Multiple Correspondence Analysis
data(BreedsDogs)

BD.act <- BreedsDogs[-7]  # active variables
BD.ilu <- BreedsDogs[7]   # ilustrative variables

FC.bd <-FactoClass( BD.act, dudi.acm, k.clust = 4,
                       scanFC = FALSE, dfilu = BD.ilu, nfcl = 10)

FC.bd

FC.bd$clus.summ
FC.bd$indices

}
\keyword{multivariate}
\keyword{cluster}
