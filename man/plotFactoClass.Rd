\name{plotFactoClass}
\alias{plotFactoClass}
\title{Factorial Planes Showing the Classes}
\description{
 For objects of class FactoClass it graphs a factorial plane showing the center of gravity of the cluster,
 and identifying with colors the cluster to which each element belongs.
}
\usage{
plotFactoClass(FC,x=1,y=2,xlim=NULL,ylim=NULL,rotx=FALSE,roty=FALSE,
                roweti=row.names(dudi$li),coleti=row.names(dudi$co),
                titre=NULL,axislabel=TRUE,col.row=1:FC$k, 
                col.col="blue",cex=0.8,cex.row=0.8,cex.col=0.8,
                all.point=TRUE,Trow=TRUE,Tcol=TRUE,cframe=1.2,ucal=0,
                cex.global=1,infaxes="out",
                nclus=paste("cl", 1:FC$k, sep=""),
                cex.clu=cex.row,cstar=1,gg=FALSE)

}
\arguments{
  \item{FC        }{ object of class \link{FactoClass}. }
  \item{x}{number indentifying the factor to be used as horizontal axis. Default x=1}
  \item{y}{number indentifying the factor to be used as vertical axis. Default y=2}
  \item{xlim}{the x limits (x1, x2) of the plot}
  \item{ylim}{the y limits of the plot}  
  \item{rotx      }{ TRUE if you want change the sign of the horizontal coordinates (default FALSE). }
  \item{roty      }{ TRUE if you want change the sign of the vertical coordinates (default FALSE). }
  \item{roweti    }{ selected row points for the graphic. Default all points.}          
  \item{coleti    }{ selected column points for the graphic. Default all points.}
  \item{titre     }{ graphics title.}                                                  
  \item{axislabel }{ if it is TRUE the axis information is written.}                 
  \item{col.row   }{ color for row points and row labels. Default \code{1:FC$k}.}           
  \item{col.col   }{ color for column points and column labels. Default "grey55".}            
  \item{cex       }{ global scale for the labels. Default cex=0.8.}                       
  \item{cex.row   }{ scale for row points and row labels. Default cex.row=0.8. }      
  \item{cex.col   }{ scale for column points and column labels. Default cex.col=0.8. }
  \item{cex.clu   }{ scale for cluster points and cluster labels. (default cex.row). }
  \item{all.point }{ if if is TRUE, all points are outlined. Default all.point=TRUE. }  
  \item{Trow      }{ if it is TRUE the row points are outlined. Default TRUE.}               
  \item{Tcol      }{ if it is TRUE the column points are outlined. Default TRUE.}            
  \item{nclus     }{ labels for the clusters (default cl1, cl2, ... }

  \item{cframe    }{ scale for graphics limits } 
  \item{ucal      }{ quality Representation Threshold in the plane. Default ucal=0}
  \item{cex.global}{ scale for the label sizes }
  \item{infaxes}{place to put the axes information: "out","in","no". Default infaxes="out".
    If infaxes="out" the graphic is similar to \code{FactoMineR} graphics, otherwise the style
    is similar to the one in \code{ade4}, without axes information when infaxes="no"}
  \item{cstar}{length of the rays between the centroids of the classes and their points}  
  \item{gg}{If TRUE the version ggplot ggrepel is perfomance. Default FALSE}
}
\details{
   It draws the factorial plane with the clusters. Only for objects \code{FactoClass}
   see \link{FactoClass}. The factorial plane is drawn with \code{planfac} and the classes 
   are projected with \code{s.class} of \code{ade4}   
}
\value{
  It draws the factorial plane x, y using $co, $li of the object of class \code{FactoClass}.
  If ucal > 0, the function inertia.dudi is used to calculate the quality of representation 
  in the plane. 
}
\author{ Campo Elias Pardo \email{cepardot@unal.edu.co}
         Pedro Cesar del Campo \email{pcdelcampon@unal.edu.co},
   
       }

\examples{


data(Bogota)
Bog.act <- Bogota[-1]
Bog.ilu <- Bogota[ 1]

FC.Bogota<-FactoClass(Bog.act, dudi.coa,Bog.ilu,nf=2,nfcl=5,k.clust=5,scanFC=FALSE)

plotFactoClass(FC.Bogota,titre="First Factorial Plane from the SCA of Bogota's Blocks",
     col.row=c("maroon2","orchid4","darkgoldenrod2","dark red","aquamarine4"))

}
\keyword{multivariate}
\keyword{cluster}
\keyword{hplot}
