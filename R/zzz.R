# Codigo para insertar en R funciones de tipo .c .C .f en formato dll en el
# directorio 'src'

.First.lib <-function(lib, pkg)
{

  
  library.dynam("FactoClass","FactoClass")


 cat("\n FactoClass: Combination of Factorial Methods and Cluster Analysis")
 cat("\n Universidad Nacional de Colombia Bogota. Departamento de Estadistica")
 cat("\n Contact: Campo Elias Pardo <cepardot@unal.edu.co> ")
 cat("\n To obtain help: ? FactoClass   \n")   
 cat("To cite FactoClass: citation(\"FactoClass\") \n" )
 }
