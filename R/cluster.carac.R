###################################################################################################
###                                                                                                            
### FUNCION PARA CARACTERIZACION DE VARIABLES EN CLUSTER  - INGLÉS                                             
### Última revisión: abril 22/08                                                                    
### Elaborado por: Pedro Cesar del Campo Neira     
###    Revisado por: Campo Elías Pardo                                                                         
###    Traducción de mensajes al inglés Campo Elías Pardo
###    Correcciones: Mauricio Sadinle                                                               
###    Universidad Nacional de Colombia                                                                        
###                                                                                                            
### cluster.carac( tabla  := objeto data.frame con variables a caracterizar de un solo tipo                    
###                (continuas o discretas)                                                                     
###        clase  := vector que determina la particion de la tabla ( as.factor(clase)=TRUE )                   
###        tipo.v := tipo de variables (continuas o discretas)                                                 
###        sign   := Nivel de significancia para la prueba estadistica... para eliminar V.test's               
###      )                                                                                                     
###                                                                                                            
###################################################################################################

cluster.carac<-function( tabla , clase , tipo.v="d" , v.lim = 2 ){

    if (!inherits(tabla, "data.frame")) 
        stop("The first argument must be an object of class data.frame") # control de objeto
 metodo <- pmatch(tipo.v , c("categoricas","nominales","discretas","continuas","frecuencia") )

 if(is.na(metodo)==TRUE)
 {return(cat("Undefined type of variables\n\n"))}

 if(metodo==1 || metodo==2 || metodo==3 ){
 
  #  VARIABLES CATEGORICAS , NOMINALES O DISCRETAS
  
  nj      <- apply ( acm.disjonctif(tabla) , 2 ,sum ) ##   cantidad de individuos mod_j
  n       <- dim(tabla)[1]                            ##   cantidad de individuos
  # --------------------------------------------------------------------------------------------------
  # función interno
  interno <- function(c.tabla)
  {     ##  Funcion para procesar en un solo cluster_k
             if(class(c.tabla)=="factor") c.tabla <- as.data.frame(c.tabla)  # Mod. Mauricio                        
             
             disy     <- acm.disjonctif(c.tabla)      ##  Tabla disyuntiva completa en cluster_k
             nk       <- dim(disy)[1]                 ##  cantidad de individuos cluster_k   
             njk      <- apply(disy,2,sum)            ##  cantidad de individuos mod_j cluster_k
  
             #  Frecuencias
              
             clas.mod <- 100*njk/nj                   ##  % de cluster_k en mod_j
             mod.clas <- 100*njk/nk                   ##  % de mod_j en cluster_k 
             Global   <- 100*nj/n                     ##  % de mod_j en n 

             ##  probabilidad hipergeometrica y valor test
             p.lim=rep(pnorm(v.lim),length(nj))
             prob <- matrix(0.5, length(nj), 1)

             #los que se caracterizan por ausencia
             Prob.AcI <- phyper(njk, nj, n - nj, nk)
             prob[Prob.AcI < 1-p.lim] <- Prob.AcI[Prob.AcI < 1-p.lim]

             #si la distribución hipergeométrica toma el valor cero con probabilidad mayor..
             #a 1-p.lim, entonces no se puede caracterizar por ausencia, por lo tanto se..
             #aumenta el umbral para caracterizar por presencia de la característica.
             p.lim[dhyper(0, nj, n - nj, nk) >= 1-p.lim] <- 2*pnorm(v.lim)-1

             #los que se caracterizan por presencia
             Prob.AcD <- phyper(njk, nj, n - nj, nk) - dhyper(njk, nj, n - nj, nk)
             prob[Prob.AcD > p.lim] <- Prob.AcD[Prob.AcD > p.lim]

             V.test <- qnorm(prob)

             SALIDA <- data.frame(Test.Value=round(V.test,3), p.Value=round(prob,3), 
             Class.Cat=round(clas.mod,1), Cat.Class=round(mod.clas,1),                           
             Global=round(Global,1), Weight = nj)                                                    
             rownames(SALIDA) <- rownames(data.frame(nj))                              
             SALIDA <- subset(SALIDA, prob != 0.5)                                     
             SALIDA <- SALIDA[order(SALIDA$Test.Value, decreasing = TRUE),]                
             return(SALIDA)
             }
        return(by(tabla, clase, interno))
    }
# Fin variables discretas 
       
 if(metodo==4){

  #  VARIABLES CONTINUAS
       
  n      <- dim(tabla)[1]                          # No. individuos
  tabla  <- data.frame(numeric(n),tabla)           # variable falsa
  mean.X <- mean(tabla)                            # media general
  S2.X   <- diag(var(tabla))                       # varianzas generales
   
  interno <- function(c.tabla){     ##  Funcion para procesar en un solo cluster_k

             nk       <- dim(c.tabla)[1]                       ## individuos en cluster_k
             mean.Xk  <- mean(c.tabla)                         ## media cluster_k
             S2.Xk    <- (n-nk)* S2.X / ( n*nk )               ## varianzas cluster_k

             V.test   <- ( mean.Xk - mean.X ) / sqrt(S2.Xk)    ## valores.test cluster_k
             
             ##-------------------------------
             SALIDA   <- data.frame(Test.Value=round(V.test,3),Class.Mean=mean.Xk,Frequency=nk,Global.Mean=mean.X)      
             ## SALIDA
 
             rownames(SALIDA)<-names(c.tabla)                   ## etiquetas variables 
             SALIDA<-SALIDA[-1,]                                ## Eliminacion variable falsa
             SALIDA <- subset(SALIDA, abs(SALIDA$Test.Value) > v.lim )    
                                                                ## salida de no representativos
             SALIDA <- SALIDA[order(SALIDA$Test.Value , decreasing = TRUE),]   ## ordena por V.test
 
             # SALIDA <- round(SALIDA,3)
              
               return(SALIDA)
            }

  return( by(tabla,clase,interno) ) # orden "by" salida para todos los cluster

 }# Fin variables continuas    

# modificación de Campo Elías Pardo

 if(metodo==5 ){
 
  #  VARIABLES DE TIPO FRECUENCIA O CONTEO
  
  nj      <- colSums(tabla)                           ##   cantidad de individuos mod_j
  n       <- sum(tabla)                               ##   frecuencia total

  interno <- function(c.tabla){     ##  Funcion para procesar en un solo cluster_k
             nk       <- sum(c.tabla)                 ##  cantidad de individuos cluster_k   
             njk      <- colSums(data.frame(c.tabla))            ##  cantidad de individuos mod_j cluster_k

             clas.mod <- 100*njk/nj                   ##  % de cluster_k en mod_j
             mod.clas <- 100*njk/nk                   ##  % de mod_j en cluster_k 
             Global   <- 100*nj/n                     ##  % de mod_j en n 

                   ##  probabilidad hipergeometrica y valor test                            
             p.lim=rep(pnorm(v.lim),length(nj))                                                    
             prob <- matrix(0.5, length(nj), 1)                                                    
                                                                                            
             #los que se caracterizan por ausencia                                                 
             Prob.AcI <- phyper(njk, nj, n - nj, nk)                                               
             prob[Prob.AcI < 1-p.lim] <- Prob.AcI[Prob.AcI < 1-p.lim]                              
                                                                                            
             #si la distribución hipergeométrica toma el valor cero con probabilidad mayor..       
             #a 1-p.lim, entonces no se puede caracterizar por ausencia, por lo tanto se..         
             #aumenta el umbral para caracterizar por presencia de la característica.              
             p.lim[dhyper(0, nj, n - nj, nk) >= 1-p.lim] <- 2*pnorm(v.lim)-1                       
                                                                                            
             #los que se caracterizan por presencia                                                
             Prob.AcD <- phyper(njk, nj, n - nj, nk) - dhyper(njk, nj, n - nj, nk)                 
             prob[Prob.AcD > p.lim] <- Prob.AcD[Prob.AcD > p.lim]                                  
                                                                                            
             V.test <- qnorm(prob)                                                                 
             SALIDA <- data.frame(Test.Value=round(V.test,3), p.Value=round(prob,3), 
             Class.Cat=round(clas.mod,1), Cat.Class=round(mod.clas,1),                           
             Global=round(Global,1), Weight = nj)                                                    
             rownames(SALIDA) <- rownames(data.frame(nj))                              
             SALIDA <- subset(SALIDA, prob != 0.5)                                     
             SALIDA <- SALIDA[order(SALIDA$Test.Value, decreasing = TRUE),]                
             return(SALIDA)                                                                        
             }

   return( by(tabla,clase,interno) ) # orden "by" salida para todos los cluster 

 }# End of counting or frequency variables 
}
# END OF THE FUNCTION #
