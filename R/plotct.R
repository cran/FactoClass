#---------------------------------------------------------
# Función para graficar perfiles fila y columna de tablas
# de contingencia
# versión inicial de Camilo Torres
# Modificaciones julio 15 de 2010. CE Pardo
#---------------------------------------------------------
plotct <- function(x,profiles="both",legend.text=TRUE,... )
{
 x <- as.matrix( x )
 total <- sum( x )
 f.marginal <- colSums( x ) / total
 c.marginal <- rowSums( x ) / total
 f.perfil <- rbind( prop.table( x, 1 ), marginal=f.marginal )
 c.perfil <- cbind( prop.table( x, 2 ), marginal=c.marginal )
 # graficas con leyenda
 if (legend.text==TRUE)
   {
      if (profiles=="both" | profiles=="row")
       barplot( t(f.perfil), legend.text=legend.text, beside=FALSE, horiz=TRUE,
               las=1, xlim=c(0,1.5),
               xaxp=c(0,1,5),
               args.legend = list( x = "right"), ... )
     if (profiles=="both") dev.new()
     if (profiles=="both" | profiles=="col")
       barplot( c.perfil, legend.text=legend.text, beside=FALSE, las=2,
               xlim=c(0,ncol(x)+6.5),
               args.legend = list( x = "right" ), ... )
  }
  # graficas sin leyenda
 if (legend.text==FALSE)
   {
      if (profiles=="both" | profiles=="row")
       barplot( t(f.perfil),beside=FALSE, horiz=TRUE,las=1, ... )
     if (profiles=="both") dev.new()
     if (profiles=="both" | profiles=="col")
       barplot( c.perfil,beside=FALSE, las=2,... )
  }
}

