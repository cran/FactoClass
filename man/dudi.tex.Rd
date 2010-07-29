\encoding{latin1}
\name{dudi.tex}
\alias{dudi.tex}
\alias{latex}
\title{LaTeX Tables of Coordinates and Aids to Interpretation of Principal Axis Methods}
\description{Coordinates and aids of interpretation are wrote in
tabular environment of LaTeX inside a Table
}
\usage{dudi.tex(dudi,job="",aidsC=TRUE,aidsR=TRUE,append=TRUE)
latex(obj,job="latex",tit="",lab="",append=TRUE,dec=1)
}

\arguments{
  \item{dudi}{an object of class \code{dudi}}
  \item{job}{a name to identify files and outputs}
  \item{aidsC}{if it is TRUE the coordinates and aids of
  interpretation of the columns are printed}
  \item{aidsR}{if it is TRUE the coordinates and aids of
  interpretation of the rows are printed}
  \item{append}{if it is TRUE LaTeX outputs are appended on the file}
  \item{obj}{object to export to LaTeX}
  \item{tit}{title of the table}
  \item{lab}{label for crossed references of LaTeX table}
  \item{dec}{number of decimal digits}
}

\details{
\code{latex} function is used to builp up a table. The aids
of interpretation are obtained with \code{inertia.dudi} of
\code{ade4}.
A file is wrote in the work directory (\code{job.txt}) with the following tables:

\describe{
   \item{tvalp}{eigenvalues}
   \item{c1}{eigenvectors}
   \item{co}{column coordinates}
   \item{col.abs}{column contributions in percentage}
   \item{col.rel}{quality of the representation of columns in percentage}
   \item{col.cum}{accumulated quality of the representation of columns in percentage/100}
   \item{li}{row coordinates}
   \item{row.abs}{row contributions in percent}
   \item{row.rel}{quality of the representation of rows in percentage}
   \item{row.cum}{accumulated quality of the representation of rows in percentage/100}
}
}
\author{
Campo Elías PARDO \email{cepardot@unal.edu.co} }
\examples{
data(ardeche)
coa1 <- dudi.coa(ardeche$tab, scann = FALSE, nf = 4)
dudi.tex(coa1,job="Ardeche") 
}
\keyword{multivariate}
