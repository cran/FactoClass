\name{ward.cluster}
\alias{ward.cluster}
\title{Hierarchic Classification by Ward's Method}
\description{
 Performs the classification by Ward's method from the matrix of Euclidean distances.
}
\usage{
ward.cluster(dista, peso = NULL , plots = TRUE, h.clust = 2, n.indi = 25 )
}
\arguments{
  \item{dista   }{  matrix of Euclidean distances ( class(dista)=="dist" ). }
  \item{peso    }{ (Optional) weight of the individuals, by default equal weights}
  \item{plots   }{  it makes dendrogram and histogram of the Ward's method}
  \item{h.clust }{ if it is '0' returns a object of class \code{hclust} and a table of level indices,
                   if it is '1' returns a object of class \code{hclust},
                   if it is '2' returns a table of level indices.}
   \item{n.indi }{ number of indices to draw in the histogram (default 25).}
}
\details{
It is an entrance to the function \code{h.clus} to obtain the results of the procedure presented in
Lebart et al. (1995). Initially the matrix of distances of Ward of the elements to classify is calculated:

The Ward's distance between two elements to classify $i$ and $l$ is given by:

       \deqn{W(i,l) = (m_i * m_l)/(m_i + m_i) * dist(i,l)^2}
       where $m_i$ y $m_l$ are the weights and $dist(i,l)$ is the Euclidean distance between them.

}
\value{ 
 It returns an object of class hclust and a table of level indices (depending of h.clust). 
 If plots = TRUE it draws the indices of level and the dendrogram.
 
}
\author{ Pedro Cesar del Campo \email{pcdelcampon@unal.edu.co},
         Campo Elias Pardo \email{cepardot@unal.edu.co}
}

\references{
Lebart, L. and Morineau, A. and Piron, M. (1995) \emph{Statisitique exploratoire
multidimensionnelle}, Paris.
}

\examples{
data(ardeche)
ca <- dudi.coa(ardeche$tab,scannf=FALSE,nf=4)

 ward.cluster( dista= dist(ca$li), peso=ca$lw )

 dev.new()
 HW <- ward.cluster( dista= dist(ca$li), peso=ca$lw ,h.clust = 1)
 plot(HW)
 rect.hclust(HW, k=4, border="red")

}
\keyword{multivariate}
\keyword{hplot}

