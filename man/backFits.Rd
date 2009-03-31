\name{backFits}
\alias{backFits}
\alias{backFits-class}
\title{ Backfitted values for a list of Richards curves }
\description{ Backfitted values are calculated,
              with fit of the curves, for a list of Richards curves. }
\usage{
backFits(object, FUN, ...)
}
\arguments{
  \item{object}{ a list of datasets. }
  \item{FUN}{ a function estimating a model for each dataset. }
  \item{\dots}{ additional arguments. }
}
\details{ Calculates the backfitted values for a list of Richards curves,
          with computing the fits of the curves. }
\value{ List of matrices with relevant values, see \link{backFit}. }
\author{ Jens Henrik Badsberg }
\keyword{ package }
