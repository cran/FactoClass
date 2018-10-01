\encoding{latin1}
\name{plotcc}
\alias{plotcc}
\title{Correlation circle from coordinates}
\description{
 It plots Correlation circle from a coordinate table
}
\usage{
plotcc(x,ex=1,ey=2,cex.label=4.5,col.label="black",font.label="bold",col.arrow="black",
			fullcircle=TRUE,y=NULL)
}
\arguments{
  \item{x}{matrix or data.frame with coordinates}
  \item{ex}{the component like horizontal axis}
  \item{ey}{the component like vertical axis}
  \item{cex.label}{size of the variable labels. Default 4.5}
  \item{col.label}{color of the variable labels. Default black}
  \item{font.label}{font of the variable labels from fontface of ggplot2. Defult bold}                     
  \item{col.arrow}{color of the arrows. Default black}
  \item{fullcircle}{if it is TRUE (default), the circle is complete}
  \item{y}{internal}
  }
\details{
  Plot the selected factorial plane as a correlation circle for the variables from a normed PCA. 
}
\value{
  It graphs the factorial plane ex,ey using a data.frame or matrix x with axis coordinates.
}
\author{Jhonathan Medina  \email{jmedinau@unal.edu.co} and
        Campo Elias Pardo \email{cepardot@unal.edu.co}
}
\examples{
data(admi)
pca <- dudi.pca(admi[,2:6],scannf=FALSE,nf=2)
# fullcircle
plotcc(pca$co)
# no fullcircle
plotcc(pca$co,fullcircle=FALSE)
}
\keyword{multivariate}
\keyword{hplot}

