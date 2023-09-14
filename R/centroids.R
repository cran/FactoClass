# -------------------------------------------
# Funcion para calcular centroides de una particion
# y razon de correlaciones abril 11 2017
# codigo tomado de funcion FactoClasss.class de ade4
# julio 7 de 2009
# Campo E. pardo
# Entra particion, coordenadas y pesos
# salen coordenadas de los centroides
# razones de correlaciones 
# ---------------------------------------------
centroids <- function(df,cl,rw=rep(1/nrow(df),nrow(df)))
{
  if (!inherits(df, c("numeric","matrix","data.frame")))  stop("Object of class  numeric, matrix or data.frame expectes")
  if (!inherits(cl,"factor")) stop("Object of class factor expectes")    
  if (!all(unlist(lapply(df, is.numeric)))) 
    stop("All variables must be numerics")
  if (inherits(df,"numeric")) {
    df<- as.data.frame(df)
    names(df)<-"x"
  }
  f1 <- function(cl)
  {
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    data.frame(x)
  }
  f1(cl)  # TDC de la particion
  dfdistri <- f1(cl) * rw  
  w1 <- unlist(lapply(dfdistri, sum)) # pesos de las clases
  coo <- as.matrix(t(dfdistri)) %*%as.matrix(df)/w1
  if (is.factor(cl)) rownames(coo) <-levels(cl)
  xmed<-colMeans(df)
  var<- diag(var(df))*(nrow(df)-1)/nrow(df)
  vark <- w1 %*% (coo - rep(1,length(levels(cl)))%*%t(xmed))^2 
  cr <-vark/var
  return(list(centroids=coo,weights=w1,cr=cr))
}                                         


