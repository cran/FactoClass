# Codigo para insertar en R funciones de tipo .c .C .f en formato dll en el
# directorio 'src'

.First.lib <-function(lib, pkg)
{

  
  library.dynam("FactoClass","FactoClass")


 cat("\n FactoClass: Combination of Factorial Methods and Cluster Analysis")
 cat("\n Universidad Nacional de Colombia Bogota. Departamento de Estadistica")
 cat("\n Programed by: Campo Elias Pardo <cepardot@unal.edu.co> ")
 cat("\n               Pedro Cesar del Campo <pcdelcampon@unal.edu.co>")    
  cat("\n With the contribution of:"      )
 cat("\n               Ivan Diaz <ildiazm@unal.edu.co>  ")
 cat("\n               Mauricio Sadinle <msadinleg@unal.edu.co>") 
 cat("\n English revision by:")
 cat("\n               Diana Carolina Zarate <dczarated@unal.edu.co>   \n")  
 cat("\n To obtain help: ? FactoClass   \n")   
 cat("To cite FactoClass: citation(\"FactoClass\") \n" )
 }
