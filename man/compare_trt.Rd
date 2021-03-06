% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_trt.R
\name{compare_trt}
\alias{compare_trt}
\title{Compare Treatments}
\usage{
compare_trt(outcome, plotID, trtID, grpID = NULL, trt1, trt2, smoother = T,
  meanline = T)
}
\arguments{
\item{outcome}{vector containing numeric outcome values}

\item{plotID}{ID variable for plot/rep/individual}

\item{trtID}{ID variable for treatment}

\item{grpID}{optional ID variable for grouping factor to set colors by}

\item{trt1}{string indicating which treatment to plot on x axis}

\item{trt2}{string indicating which treatment to plot on y axis}

\item{smoother}{add geom with loess smoother}

\item{meanline}{add geom with overall mean difference parallel to y=x}
}
\description{
Plot comparing two treatments tested in multiple identical locations from long format data
}
\examples{
library(agricolae)
data(plrv)

#example without groups
compare_trt(outcome=plrv$Yield,
plotID=paste(plrv$Locality,plrv$Rep),
trtID=plrv$Genotype,
trt1="Canchan",
trt2="Desiree")

#example with groups
compare_trt(outcome=plrv$Yield,
plotID=paste(plrv$Locality,plrv$Rep),
trtID=plrv$Genotype,
grpID=plrv$Locality,
trt1="Canchan",
trt2="Desiree")
}
\keyword{Compare}
\keyword{Plot}
