#---------------------------------------------------------
# Función para graficar diagrams de dispersion
# con las densidades kernel en la diagonal
# las densidades kernel bivariadas en la diagonal superior
# diagrams de dispersión en diagonal inferior
#  abril 12  de 2017. CE Pardo
#---------------------------------------------------------
plotpairs <- function(X,maxg=5,cex=1){
  if (!all(unlist(lapply(X, is.numeric)))) 
    stop("All variables must be numerics")
  if (ncol(X)>maxg) {
    X <- X[,1:maxg]
    warning("Only the first ",maxg, " columns are plotted")
  }
  nvar<-ncol(X)
  n <- nrow(X)
# modificado de Hardle y Simar  
  i=0
  op = par(mfrow=c(nvar, nvar),cex=cex/nvar,las=1)#,cex.axis=cex/nvar)
  # while1
  while(i<nvar){
    i = i+1
    j = 0
    # while2 en while1
    while(j<nvar){
      j = j+1
      # if1 en while 2
      if(i==j){
        #plot(i, type="n", axes=FALSE, xlab="", ylab="", main=i, cex.main=5)
        var<-i
        xy1<-density(X[,var])
#        xy2<-density(iris[51:100,var])
#        xy3<-density(iris[101:150,var])
        x<-xy1$x #rbind(xy1$x,xy2$x,xy3$x)
        y<-xy1$y #rbind(xy1$y,xy2$y,xy3$y)
        plot(x,y,type="n",xlab=colnames(X)[var],main="",cex.lab=cex*0.8*nvar,cex.axis=cex*0.4*nvar)
        lines(xy1,col=1,lwd=2)
        #lines(xy2,col=2,lwd=2,lty=2)
        #lines(xy3,col=3,lwd=2,lty=3)
      } # fin if1 en while 2
      # if2 en while2
      if(i<j){
        xx = cbind(X[,i],X[,j],c(rep(1,n),rep(1,n)))
        zz = bkde2D(xx[,-3], 0.4)
        contour(zz$x1, zz$x2, zz$fhat, nlevels=12, col=rainbow(20), 
                drawlabels=FALSE, xlab="X", ylab="Y",cex.axis=cex*0.4*nvar)
        }# fin if2 en while2
      #if3 en while2
      if(i>j){
        yy = cbind(X[,i],X[,j],c(rep(1,n),rep(1,n)))
        plot(yy[,-3], pch=as.numeric(yy[,3]), xlab="X", ylab="Y",cex.axis=cex*0.4*nvar)#, col=iris[,5])
        } # fin if3 en while2
    } # fin while2
    } # fin while1
  par(op)
} # fin funcion
#------------------------------------  
