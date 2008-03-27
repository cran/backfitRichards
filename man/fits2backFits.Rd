\name{fits2backFits}
\alias{fits2backFits}
\title{ Backfitted values from fits of Richards curves }
\description{
 Compute backfitted values from the data and the fits of Richards curves.
}
\usage{
fits2backFits(list, fits, ...)
}
\arguments{
  \item{list}{ list of datasets. }
  \item{fits}{ list of fits. }
  \item{\dots}{ additional arguments. }
}
\details{
 Compute backfitted values from the data and the fits of Richards curves,
 see \link{backFit}. The function \link{backFits} does not have the argument
 \code{fit}, and has to fit the models. This function \code{fits2backFits}
 takes the fits as argument. The backfitted values are the found by applying
 \link{backFit} to the i-te element of \code{list} and the i-te element of
 \code{fit}. Thus the two list has to match.
}
\value{ List of matrices with relevant values.}
\author{ Jens Henrik Badsberg }
\keyword{ package }
