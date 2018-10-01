#---------------------------------------------------------------------------------------------
# grafica del circulo de correlaciones
# Jhonathan Medina 
# Marzo 30 2018
# requiere ggplot2, ggrepel
#PARAMETROS (entre parentesis valores por defecto)
#   x es un data frame con las coordenadas de las variables en el plano factorial
#   ex,ey ejes a graficar (1,2)
#   cex.axis: tama?o de los nombres de los ejes (12)
#   cex.label: tama?o de las etiquetas de las variables (4.5)
#   col.label: color de las etiquetas de las variables (black)
#   font.label: se elige el tipo de letra para las etiquetas de las variables, todos los posibles casos son los 
#		    mismos que en 'fontface' de ggplot2 (bold)
#   col.arrow: color de los vectores de las variables (black)
#   fullcircle: especifica si se ve el circulo unitario completo o un se realiza un zoom sobre el area donde se ubican las
#		    variables (TRUE)
#---------------------
#REFERENCES
#	joran. (2011, July 28). Re: Draw a circle with ggplot2 [Blog comment]. Retrieved from https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
#---------------------------------------------------------------------------------------------

plotcc<-function(x,ex=1,ey=2,cex.label=4.5,col.label="black",font.label="bold",
                 col.arrow="black",fullcircle=TRUE,y=NULL)
  {

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
} # joran(2011)

	x<-x[,c(ex,ey)]
	if(!is.null(col.arrow))x<-data.frame(x,col.arrow)
	names(x)<-c("ex","ey")
	circle<-circleFun(c(0,0),2,npoints = 1000)

	if(!fullcircle){cxlim=c(min(x[,1])-0.05,max(x[,1])+0.05) 
	if(prod(cxlim)>0) (if(abs(cxlim[1])<abs(cxlim[2])) {cxlim[1]=0;cxlim[2]=sign(cxlim[2])*1} else{cxlim[2]=0;cxlim[1]=sign(cxlim[1])*1}  )}else{cxlim=c(-1,1)}

	if(!fullcircle){cylim=c(min(x[,2])-0.05,max(x[,2])+0.05) 
	if(prod(cylim)>0) (if(abs(cylim[1])<abs(cylim[2])) (cylim[1]=0) else{cylim[2]=0}  )}else{cylim=c(-1,1)}


ggplot(x,aes(x=ex,y=ey))+
	geom_path(data=circle,aes(x,y))+
	geom_text_repel(aes(label=rownames(x)),fontface=font.label,size=cex.label,colour=col.label)+
	geom_point(aes(x=ex,y=ey),size=0.5)+
	geom_segment(aes(x=0,y=0,xend=x[,1],yend=x[,2]),colour=col.arrow,arrow=arrow(length=unit(0.03,"npc")))+
	xlim(cxlim)+ylim(cylim)+theme_void()+
	geom_segment(aes(x=cxlim[1],y=0,xend=cxlim[2],yend=0),linetype=2)+
	geom_segment(aes(x=0,y=cylim[1],xend=0,yend=cylim[2]),linetype=2) 
}



