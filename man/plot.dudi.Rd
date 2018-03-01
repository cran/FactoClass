\encoding{latin1}
\name{plot.dudi}
\alias{plot.dudi}
\alias{sutil.grid}
\title{Factorial Planes from Objects of Class dudi}
\description{
 It plots factorial planes from objects of class \code{dudi}
}
\usage{
\method{plot}{dudi}(x,ex=1,ey=2,xlim=NULL,ylim=NULL,main=NULL,rotx=FALSE,roty=FALSE,
			roweti=row.names(dudi$li),coleti=row.names(dudi$co),
			axislabel=TRUE,col.row="black",col.col="blue",cex=0.8,
			cex.row=0.8,cex.col=0.8,all.point=TRUE,Trow=TRUE,Tcol=TRUE,
			cframe=1.2,ucal=0,cex.global=1,infaxes="out",gg=FALSE,\dots)
sutil.grid(cgrid,scale=TRUE)    
}
\arguments{
  \item{x}{ object of type dudi}
  \item{ex}{number indentifying the factor to be used as horizontal axis. Default 1}
  \item{ey}{number indentifying the factor to be used as vertical axis. Default 2}
  \item{xlim}{the x limits (x1, x2) of the plot}
  \item{ylim}{the y limits of the plot}  
  \item{main}{graphic title}
  \item{rotx}{TRUE if you want change the sign of the horizontal coordinates. Default FALSE }
  \item{roty}{TRUE if you want change the sign of the vertical coordinates. Default FALSE }
  \item{roweti}{selected row points for the graphic. Default all points}
  \item{coleti}{selected column points for the graphic. Default all points}
   \item{axislabel}{if it is TRUE the axis information is written}
  \item{col.row}{color for row points and row labels. Default "black"}
  \item{col.col}{color for column points and column labels. Default "blue"}
  \item{cex}{global scale for the labels. Default cex=0.8}
  \item{cex.row}{scale for row points and row labels. Default cex.row=0.8 }
  \item{cex.col}{scale for column points and column labels. Default cex.col=0.8 }
  \item{all.point}{If if is TRUE, all points are outlined. Default all.point=TRUE }
  \item{Trow}{if it is TRUE the row points are outlined. Default TRUE}
  \item{Tcol}{if it is TRUE the column points are outlined. Default TRUE}
  \item{cframe}{scale for graphic limits}
  \item{ucal}{quality representation threshold (percentage) in the plane . Default ucal=0}
  \item{cex.global}{scale for the label sizes }
  \item{infaxes}{place to put the axes information: "out","in","no". Default infaxes="out".
    If infaxes="out" the graphic is similar to \code{FactoMineR} graphics, otherwise the style
    is similar to the one in \code{ade4}, without axes information when infaxes="no"}
  \item{gg}{If TRUE the version ggplot ggrepel is perfomance. Default TRUE }
  \item{\dots}{further arguments passed to or from other methods}
  \item{cgrid}{internal parameter}
  \item{scale}{internal}  
}
\details{
  Plot the selected factorial plane. 
  \code{sutil.grid} is used by \code{plot.dudi} 
}
\value{
  It graphs the factorial plane x,y using  $co, $li of a "dudi" object. 
  If ucal > 0, the function inertia.dudi is used to calculate the quality of representation 
  on the plane 
}
\author{Campo Elias Pardo \email{cepardot@unal.edu.co} and 
        Jhonathan Medina  \email{jmedinau@unal.edu.co}
}
\examples{
data(Bogota)
ca <- dudi.coa(Bogota[,2:7],scannf=FALSE,nf=4)
# FactoMineR style
plot(ca)
dev.new()
# ade4 style
plot.dudi(ca,ex=3,ey=4,ucal=0.2,all.point=FALSE,infaxes="in",gg=FALSE)
}
\keyword{multivariate}
\keyword{hplot}

