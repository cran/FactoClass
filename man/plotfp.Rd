\encoding{latin1}
\name{plotfp}
\alias{plotfp}
\title{Factorial Planes from Coordinates}
\description{
 It plots factorial planes from a coordinate table
}
\usage{
plotfp(co,x=1,y=2,eig=NULL,cal=NULL,ucal=0,xlim=NULL,ylim=NULL,main=NULL,rotx=FALSE,
       roty=FALSE,eti=row.names(co),axislabel=TRUE,col.row="black",cex=0.8,cex.row=0.8,
       all.point=TRUE,cframe=1.2,cex.global=1,infaxes="out",asp=1,gg=FALSE)
}
\arguments{
  \item{co}{matrix or data.frame with coordinates}
  \item{x}{the component like horizontal axis}
  \item{y}{the component like vertical axis}
  \item{eig}{numeric with the eigenvalues}
  \item{cal}{matrix or data.frame with the square cosinus}
  \item{ucal}{quality representation threshold (percentage) in the plane . Default ucal=0}
  \item{xlim}{the x limits (x1, x2) of the plot}
  \item{ylim}{the y limits of the plot}  
  \item{main}{graphic title}
  \item{rotx}{TRUE if you want change the sign of the horizontal coordinates. Default FALSE }
  \item{roty}{TRUE if you want change the sign of the vertical coordinates. Default FALSE }
  \item{eti}{selected row points for the graphic. Default all points}
  \item{axislabel}{if it is TRUE the axis information is written}
  \item{col.row}{color for row points and row labels. Default "black"}
  \item{cex}{global scale for the labels. Default cex=0.8}
  \item{cex.row}{scale for row points and row labels. Default cex.row=0.8 }
  \item{all.point}{If if is TRUE, all points are outlined. Default all.point=TRUE }
  \item{cframe}{scale for graphic limits}
  \item{cex.global}{scale for the label sizes }
  \item{infaxes}{place to put the axes information: "out","in","no". Default infaxes="out".
    If infaxes="out" the graphic is similar to \code{FactoMineR} graphics, otherwise the style
    is similar to the one in \code{ade4}, without axes information when infaxes="no"}
 \item{asp}{the y/x aspect ratio}
   \item{gg}{If TRUE the version ggplot ggrepel is perfomance. Default FALSE}
}
\details{
  Plot the selected factorial plane. 
}
\value{
  It graphs the factorial plane x,y using  co and optional information of eigenvalues and representation quality of the points.
  If ucal > 0, only the points with  the quality of representation 
  on the plane bigger than ucal are pointed
}
\author{Campo Elias Pardo \email{cepardot@unal.edu.co} and 
        Jhonathan Medina  \email{jmedinau@unal.edu.co}
}
\examples{
data(Bogota)
ca <- dudi.coa(Bogota[,2:7],scannf=FALSE,nf=2)
# ade4 style
plotfp(ca$li,eig=ca$eig,main="First Factorial Plane",infaxes="in")
# with ggplot2 and ggrepel
plotfp(ca$li,eig=ca$eig,main="First Factorial Plane",gg=TRUE)
}
\keyword{multivariate}
\keyword{hplot}

