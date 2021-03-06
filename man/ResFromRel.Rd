% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/popdyn_MICE.R
\name{ResFromRel}
\alias{ResFromRel}
\title{Returns Results of a set of MICE relationships}
\usage{
ResFromRel(Rel, Bcur, SSBcur, Ncur, seed)
}
\arguments{
\item{Rel}{A list of inter-stock relationships see slot Rel of MOM object class}

\item{Bcur}{An array of current stock biomass [stock, age, area]}

\item{SSBcur}{An array of current spawning stock biomass [stock, age, area]}

\item{Ncur}{An array of current stock numbers [stock, age, area]}
}
\description{
Predicts stock-specific parameters from another stocks biomass, spawning biomass or numbers
}
\author{
T.Carruthers
}
\keyword{internal}
