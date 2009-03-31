\name{backFit}
\alias{backFit}
\alias{backFit-class}
\title{ Backfitted values for one Richards curve }
\description{ Calculate the backfitted values for one Richards curve. }
\usage{
backFit(X, FUN = NULL, fit = NULL, ny = 0,
        respName = "SIGNAL", indepName = "ARGX",
        applyFUN2X = function(X, FUN, ...) FUN(X, ...), ...) 
}
\arguments{
  \item{X}{ the dataset for \code{FUN}. }
  \item{FUN}{ a function estimating a model for each dataset.
              This function is only called if the argument
              \code{fit} is not given. }
  \item{fit}{ a optional fit. }
  \item{ny}{ a (optional) numerical value. This value has to given,
             if \code{ny} is not fitted in the model. }
  \item{respName}{ a text string with the name of the dependent variable. }
  \item{indepName}{ a text string with the name of the independent variable.. }
  \item{applyFUN2X}{ a function applied to each dataset,
                     retrieving starting value and fixing outliers,
                     and then calling the function \code{FUN} to
                     estimate the model for the modified dataset. 
                     This function is only called if the argument
                     \code{fit} is not given. }
  \item{\dots}{ additional arguments. }
}
\details{
The backfitted values for one Richards curve are calculated: For each
value of the independent variable (\code{indepName}) the observed mean of
the dependent variable (\code{respName}) is found from the dataset \code{X}.  
The value is found of the independent variable for which the value of the
estimated Richards function is equal to the observed mean of the response,
by the inverse of the Richards function and the fit of the curve.
This estimated value of the independent variable is taken relative the
actual value of the independent variable.
}
\value{Matrix with relevant values. }
\author{ Jens Henrik Badsberg }
\keyword{ package }
