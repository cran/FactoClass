\name{chisq.carac}
\alias{chisq.carac}
\title{Chisquare tests of a qualitative variable by several qualitative variables }
\description{Chisqure tests are performed for the contingency tables crossing a qualitative variable named \code{cl} and the qualitative variables present in columns from \code{df} 
}
\usage{chisq.carac(df,cl,thr=2,decr=TRUE)
}
\arguments{
  \item{df}{\code{data.frame}, with factors contain the categories of the qualitave variables}
  \item{cl}{factor indicating the category of each subject}
  \item{thr}{threshold of test value, if decr=TRUE, only   the rows where \code{tval >= thr} are returned}
  \item{decr}{if decr=TRUE the rows are returned in decreasing order}
}
\value{
Matrix  with the following columns:
\item{chi2}{chisquare statistic}
\item{dfr}{degree of fredom of chisquare densidity} 
\item{pval}{$p$ value} 
\item{tval}{quantil \code{qnorm(pval,lower.tail=FALSE}}
\item{phi2}{\code{phi2=chi2/n}}
}
\author{Campo Elias Pardo \email{cepardot@unal.edu.co}
}
\examples{
data(DogBreeds)
round(chisq.carac(DogBreeds[,-7],DogBreeds[,7]),3)
round(chisq.carac(DogBreeds[,-7],DogBreeds[,7],decr=FALSE),3)
}
\keyword{multivariate}
