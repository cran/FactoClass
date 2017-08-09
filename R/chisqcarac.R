#------------------------------------------#
# chisq,test para varias tablas para carac #
# una variable cualitativa                 #
# Campo Elías Pardo  Abril 11 2017         #
#                                          #
# -----------------------------------------#
chisq.carac <- function(df,cl,thr=2,decr=TRUE){
  if (!all(unlist(lapply(df, is.factor)))) 
    stop("All variables must be factors")
  df <- as.data.frame(df)
  nvar<-ncol(df)
  # calculos por tabla

  tabex <- NULL
  for (t in 1:nvar){
    suppressWarnings(chisq.test(df[,t],cl)) -> chist
    tval<- qnorm(chist$p.value,lower.tail=FALSE)
    phi2 <- as.numeric(chist$statistic/nrow(df))
    tabex <- rbind(tabex,c(chi2=as.numeric(chist$statistic),
                        dfr=as.numeric(chist$parameter),
                        pval = chist$p.value,
                        tval = tval,
                        phi2 = phi2) 
                        )
  }
  rownames(tabex) <- colnames(df)
  if (decr) {
    tabex <- tabex[tabex[,4]>=thr,]
    return(tabex[order(tabex[,4],decreasing = TRUE),])
  }  
  else return(tabex)
}