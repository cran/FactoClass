pkgname <- "FactoClass"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('FactoClass')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Fac.Num")
### * Fac.Num

flush(stderr()); flush(stdout())

### Name: Fac.Num
### Title: Division of qualitative and quantitative variables.
### Aliases: Fac.Num Fac.Num
### Keywords: multivariate

### ** Examples


   data(BreedsDogs)
   Fac.Num(BreedsDogs)

   data(iris)
   Fac.Num(iris)




cleanEx()
nameEx("FactoClass")
### * FactoClass

flush(stderr()); flush(stdout())

### Name: FactoClass
### Title: Combination of Factorial Methods and Cluster Analysis
### Aliases: FactoClass print.FactoClass analisis.clus
### Keywords: multivariate cluster

### ** Examples


# Cluster analysis with Correspondence Analysis
data(ColorAdjective)
FC.col <-FactoClass(ColorAdjective, dudi.coa)
6
10
5

FC.col

FC.col$dudi


# Cluster analysis with Multiple Correspondence Analysis
data(BreedsDogs)

BD.act <- BreedsDogs[-7]  # active variables
BD.ilu <- BreedsDogs[7]   # ilustrative variables

FC.bd <-FactoClass( BD.act, dudi.acm, k.clust = 4,
                       scanFC = FALSE, dfilu = BD.ilu, nfcl = 10)

FC.bd

FC.bd$clus.summ
FC.bd$indices




cleanEx()
nameEx("FactoClass.tex")
### * FactoClass.tex

flush(stderr()); flush(stdout())

### Name: FactoClass.tex
### Title: Table of Coordinates, Aids of Interpretation of the Principal
###   Axes and Cluster Analysis in LaTeX format.
### Aliases: FactoClass.tex print.FactoClass.tex latexDF roundDF
### Keywords: multivariate

### ** Examples

data(BreedsDogs)
BD.act <- BreedsDogs[-7]  # active variables
BD.ilu <- BreedsDogs[7]   # illustrative variables
# MCA
FaCl <- FactoClass( BD.act, dudi.acm,
                    scanFC = FALSE, dfilu = BD.ilu, nfcl = 10, k.clust = 4 )
FactoClass.tex(FaCl,job="BreedsDogs1", append=TRUE)
FactoClass.tex(FaCl,job="BreedsDogs", append=TRUE , p.clust = TRUE)



cleanEx()
nameEx("centroids")
### * centroids

flush(stderr()); flush(stdout())

### Name: centroids
### Title: Centroids of the Classes of a Partition
### Aliases: centroids
### Keywords: multivariate

### ** Examples

data(iris)
centroids(iris[,-5],iris[,5])



cleanEx()
nameEx("cluster.carac")
### * cluster.carac

flush(stderr()); flush(stdout())

### Name: cluster.carac
### Title: Cluster Characterization by Variables.
### Aliases: cluster.carac cluster.carac
### Keywords: multivariate hplot

### ** Examples


data(BreedsDogs)
BD.act <- BreedsDogs[-7]  # active variables
BD.function <- subset(BreedsDogs,select=7)   
cluster.carac(BD.act,BD.function,"ca",2.0)  #  nominal variables


data(iris)
iris.act <- Fac.Num(iris)$numeric
clase <- Fac.Num(iris)$factor
cluster.carac(iris.act,clase,"co",2.0)  #  continuous variables

# frequency variables
data(BreedsDogs)
attach(BreedsDogs)
weig<-table(FUNc,WEIG)
weig<-data.frame(weig[,1],weig[,2],weig[,3])
cluster.carac(weig,  row.names(weig), "fr", 2) # frequency variables
detach(BreedsDogs)



cleanEx()
nameEx("dudi.tex")
### * dudi.tex

flush(stderr()); flush(stdout())

### Name: dudi.tex
### Title: LaTeX Tables of Coordinates and Aids to Interpretation of
###   Principal Axis Methods
### Aliases: dudi.tex latex
### Keywords: multivariate

### ** Examples

data(ardeche)
coa1 <- dudi.coa(ardeche$tab, scann = FALSE, nf = 4)
dudi.tex(coa1,job="Ardeche") 



cleanEx()
nameEx("kmeansW")
### * kmeansW

flush(stderr()); flush(stdout())

### Name: kmeansW
### Title: K-means with Weights of the Elements
### Aliases: kmeansW kmeansW
### Keywords: multivariate cluster

### ** Examples

 data(Bogota)
 ac.bog <- Bogota[-1]
 il.bog <- Bogota[ 1]
 
 acs <- dudi.coa( ac.bog, nf=6 , scannf = FALSE )
 
 kmeansW( acs$li, 7, acs$lw )
 



cleanEx()
nameEx("list.to.data")
### * list.to.data

flush(stderr()); flush(stdout())

### Name: list.to.data
### Title: list to data.frame
### Aliases: list.to.data list.to.data
### Keywords: multivariate

### ** Examples


 A <- data.frame(r1=rnorm(5),r2=rnorm(5))
 B <- data.frame(r1=rnorm(15),r2=rnorm(15))

 LL <- list(A=A,B=B)
 LL
 list.to.data(LL)





cleanEx()
nameEx("planfac")
### * planfac

flush(stderr()); flush(stdout())

### Name: planfac
### Title: Correspondence Analysis Factorial Planes
### Aliases: planfac sutil.grid
### Keywords: multivariate hplot

### ** Examples

data(ardeche)
ca <- dudi.coa(ardeche$tab,scannf=FALSE,nf=4)
# FactoMineR style
planfac(ca,ucal=40,all.point=FALSE,titre="SCA of Ardeche, First Factorial Plane")
dev.new()
# ade4 style
planfac(ca,x=3,y=4,ucal=20,all.point=FALSE,infaxes="in",titre="SCA of
Ardeche, Plane 3-4")



cleanEx()
nameEx("plotFactoClass")
### * plotFactoClass

flush(stderr()); flush(stdout())

### Name: plotFactoClass
### Title: Factorial Planes Showing the Classes
### Aliases: plotFactoClass
### Keywords: multivariate cluster hplot

### ** Examples



data(Bogota)
Bog.act <- Bogota[-1]
Bog.ilu <- Bogota[ 1]

FC.Bogota<-FactoClass(Bog.act, dudi.coa,Bog.ilu,nf=2,nfcl=5,k.clust=5,scanFC=FALSE)

plotFactoClass(FC.Bogota,titre="Primer plano factorial del ACS de la TC de manzanas de Bogota",
     col.row=c("maroon2","orchid4","darkgoldenrod2","dark red","aquamarine4"))




cleanEx()
nameEx("stableclus")
### * stableclus

flush(stderr()); flush(stdout())

### Name: stableclus
### Title: Stable clusters for cluster analysis
### Aliases: stableclus stableclus
### Keywords: multivariate cluster

### ** Examples

 data(ColorAdjective)
 FCcol <-FactoClass(ColorAdjective, dudi.coa,nf=6,nfcl=10,k.clust=7,scanFC = FALSE)
 acs <- FCcol$dudi
 stableclus(acs,3,3,4,TRUE,TRUE)




cleanEx()
nameEx("ward.cluster")
### * ward.cluster

flush(stderr()); flush(stdout())

### Name: ward.cluster
### Title: Hierarchic Classification by Ward's Method
### Aliases: ward.cluster ward.cluster
### Keywords: multivariate hplot

### ** Examples

data(ardeche)
ca <- dudi.coa(ardeche$tab,scannf=FALSE,nf=4)

 ward.cluster( dista= dist(ca$li), peso=ca$lw )

 dev.new()
 HW <- ward.cluster( dista= dist(ca$li), peso=ca$lw ,h.clust = 1)
 plot(HW)
 rect.hclust(HW, k=4, border="red")




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
