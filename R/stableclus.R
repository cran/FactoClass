#Algoritmo de agregación en grupos estables o formas fuertes
#Parametros: 
#dudi: Objeto de clase dudi, análisis factorial previo realizado con el paquete ade4
#part: Número de particiones o réplicas del algoritmo de medias móviles Kmeans 
#k.clust: Número inicial de clases 
#Debido a la naturaleza del programa es necesario que k.clust^part sea menor a 100000, lo cual p
#permite un gran número de clases con pocas particiones o viceversa, lo usual es un gran número de clases y un
#número reducido de particiones

stableclus = function(dudi,part=2,k.clust=2,ff.clus=NULL,bplot=TRUE,kmns=FALSE) 
{
    if(k.clust<2 || part<2) {stop("Number of partitions #Condicionales a los requisitos de la función
        and classes must be more than 2")
    }
    if((k.clust^part)>100000) {stop("Please select 
        a smaller number of partitions or clusters")
    } 
    if(class(dudi)[2]!="dudi"){stop("Not a valid dudi object")
    } 

    nf=dudi$nf                    #Extracción de los objetos necesarios del
    obj.clasf=dudi$li             #objeto dudi
    pesos=dudi$lw
    n=nrow(obj.clasf)  
    A=matrix(c(rep(0,part*(k.clust^part))),ncol=part)         
    m=nrow(A)
    cont=(k.clust^(part-1))
    i=ncol(A)-1
    c=c(1:k.clust)
    k=k.clust
    A[,part]=rep(c,cont)    #Construcción de la matriz 
    while(cont>1){          #que guarda la información 
        t=c(rep(k.clust,k)) #de todos las clasificaciones                                    
        c=rep(c,t)          #posibles para un individuo dado
        cont=cont/k.clust
        A[,i]=rep(c,cont)
        i=i-1
        k=k.clust*k
    }
    ID=c(1:m)
    cluster=matrix(c(rep(0,n*part)),ncol=part)                 #Aqui se guarda la información
 for (i in 1:part) {                                        #de la clasificación, en cada una
        kmeans=as.vector(kmeansW(x=obj.clasf,centers=k.clust,  #de las particiones, de todos los 
            weight = pesos)$cluster)                           #individuos
        cluster[,i]=kmeans  
    }
    f=c(rep(0,m))              #En esta parte se cuentan el número de individuos que pertenecen a
    ide=c(rep(0,n))            #cada una de las nuevas clases producto y se guardan estas frecuencias
    for (i in 1:m){
        for(j in 1:n){
            if(identical(cluster[j,],A[i,]) ){ 
                f[i]=f[i] + 1
                ide[j]=i
            } 
        }
    }  
    ide2=c(1:n)
    l=order(f,decreasing=TRUE) #Se procede a ordenar las frecuencias y     
    fo=sort(f,decreasing=TRUE) #se presentan en un diagrama de barras 
    fot=fo[fo>0]               #para que el usuario decida el número de clases
    IDo=ID[l]                  #finales
    IDot=IDo[fo>0]
    if(bplot) barplot(fot,las=1)   
    if (is.null(ff.clus)) {
    	cat("Select the number of clusters:")
    	ff.clus <- as.integer(readLines(n=1))
    }
    IDotf=IDot[1:ff.clus]
    IDotff=factor(IDotf)
    clsfrts=length(IDotff)                   #Ahora se procede a calcular los centros de gravedad de las                     
    cls.inc=list()                           #clases seleccionadas
    for(i in 1:clsfrts){
        cls.inc[[i]]=as.matrix(obj.clasf[ide==IDotff[i],])
    }
    f1=function(X){ tapply(X,col(X),mean) }
    c.grav=lapply(cls.inc,f1)
    C.grav=matrix(0,ncol=nf,nrow=ff.clus) 
    for(i in 1:ff.clus){
        C.grav[i,]=c.grav[[i]]
    }
    val=c(rep(0,n))
    for (i in 1:ff.clus){
        for (j in 1:n){        
            if(ide[j] != IDotff[i]){val[j]=val[j]+1}
        }   
    }         
    Reafct=obj.clasf[val==ff.clus,]            #Finalmente se agregan los demás individuos
    ide3=ide2[val==ff.clus]                    #por reafectación a las clases definitivas
    n.reafct=nrow(Reafct)                      #para esto se calculan las distancias a cada uno
    fdist=matrix(0,ncol=ff.clus,nrow=n.reafct) #de los centros de gravedad para cada individuo y se clasifica según
    for (i in 1:ff.clus){                      #la distancia mínima
        dist=(Reafct-matrix(C.grav[i,],nrow=n.reafct,ncol=nf,byrow=TRUE))^2
        fdist[,i]=as.vector(sqrt(tapply(dist,row(dist),sum)))
    } 
    class=c()
    for(i in 1:n.reafct){  
        for(j in 1:ff.clus){
            if(fdist[i,j]==min(fdist[i,])){class[i]=IDotf[j]}
        }
    }
    ide[ide3]=class                           
    names(ide) <- rownames(dudi$li)
    #ide     #El resultado es un vector que clasifica a los individuos
    # kmeans de consolidacion
    if (kmns) ide <- kmeansW(dudi$li,centroids(dudi$li,ide)$centroids)$cluster
 # objeto de salida
    sg <- list(class=ide, # vector de clases
               stacl=fot)   # grupos estables
    return(sg)                                       
}  
