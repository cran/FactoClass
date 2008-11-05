\name{planfac}
\alias{planfac}
\alias{sutil.grid}
\title{Correspondence Analysis Factorial Planes}
\description{
 It plots factorial planes of a correspondence analysis 
}
\usage{
planfac(dudi, x = 1, y = 2, rotx = FALSE, roty = FALSE, roweti=row.names(dudi$li),
                        coleti=row.names(dudi$co), titre = NULL, axislabel = TRUE, col.row = "black", 
    col.col = "blue", cex = 0.8, cex.row = 0.8, cex.col = 0.8, all.point = TRUE, Trow = TRUE, 
    Tcol = TRUE, cframe = 1.2, ucal = 0, cex.global = 1, infaxes = "out")
sutil.grid(cgrid,scale=TRUE)    
}
\arguments{
  \item{dudi}{ object of type dudi.coa or dudi.acm}
  \item{x}{number indentifying the factor to be used as horizontal axis. Default x=1}
  \item{y}{number indentifying the factor to be used as vertical axis. Default y=2}
  \item{rotx}{TRUE if you want change the sign of the horizontal coordinates. Default FALSE }
  \item{roty}{TRUE if you want change the sign of the vertical coordinates. Default FALSE }
  \item{roweti}{selected row points for the graphic. Default all points}
  \item{coleti}{selected column points for the graphic. Default all points}
  \item{titre}{graphic title}
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
  \item{...}{}
  \item{cgrid}{}
  \item{scale}{}  
}
\details{
  Plot the selected factorial plane. 
  \code{sutil.grid} is used by \code{planfac} 
}
\value{
  It graphs the factorial plane x,y using  $co, $li of a "dudi" "coa" object. 
  If ucal > 0, the function inertia.dudi is used to calculate the quality of representation 
  on the plane 
}
\author{ Campo Elias Pardo \email{cepardot@unal.edu.co} 

\url{http://www.docentes.unal.edu.co/cepardot}}
\examples{
data(ardeche)
ca <- dudi.coa(ardeche$tab,scannf=FALSE,nf=4)
# FactoMineR style
planfac(ca,ucal=40,all.point=FALSE,titre="SCA of Ardeche, First Factorial Plane")
dev.new()
# ade4 style
planfac(ca,x=3,y=4,ucal=20,all.point=FALSE,infaxes="in",titre="SCA of Ardeche, Plane 3-4")}
\keyword{multivariate}
\keyword{hplot}

