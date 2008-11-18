#---------------------------------------------------------------------------------------------
# Agrega las clases a un plano factorial
# Campo El�as Pardo, Pedro del Campo
# Octubre 11-06, modificada octubre 30/08
# requiere ade4, utiliza s.class de ade4
#PARAMETROS 
#  Los mismos parametros de planfac pero entra objeto FactoClass y par�metros nclus para nombre de
# las clases y cex.clus para tama�o de clases, col.row es un vector para color de las clases
# cstar es de s.class 0 para no poner radios a los individuos de una clase
#  infaxes: lugar para imprimir informaci�n de ejes: "out","in","no" ("out")
#---------------------------------------------------------------------------------------------
plotFactoClass <- function(FC,x=1,y=2,rotx=FALSE,roty=FALSE,roweti=row.names(dudi$li),
                        coleti=row.names(dudi$co),titre=NULL,axislabel=TRUE,
                        col.row=1:FC$k,col.col="blue",cex=0.8,cex.row=0.8,cex.col=0.8,
                        all.point=TRUE,Trow=TRUE,Tcol=TRUE,cframe=1.2,ucal=0,
                        cex.global=1,infaxes="out",
                        nclus=paste("cl", 1:FC$k, sep=""),cex.clu=cex.row,cstar=1)
{

dudi <- FC$dudi

 col.r <- numeric(nrow(dudi$li))
 for(i in 1:FC$k){ col.r[ as.numeric(FC$cluster) == i ]  <-  col.row[i]   }
 
 names(col.r) <-  names(FC$cluster)
colpunto <- "black"

planfac(FC$dudi,x=x,y=y,rotx,roty,roweti,
                        coleti,titre,axislabel,
                        col.row=colpunto,col.col,cex,cex.row,cex.col,
                        all.point,Trow,Tcol,cframe,ucal,
                        cex.global,infaxes)
# grafica de las clases
     if (rotx) rotx=-1 else rotx=1
     if (roty) roty=-1 else roty=1
     corli <- cbind(rotx*dudi$li[,x],roty*dudi$li[,y])
     
     s.class(corli,xax=x,yax=y,fac=FC$cluster,wt=dudi$lw,add.plot = TRUE,
              cstar = cstar, cellipse = 0,
              label=nclus,clabel=cex.clu,col=col.row)
 
}
#---------------------------------------------------------------------------------------------

