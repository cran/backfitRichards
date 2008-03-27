\name{backfitRichards-package}
\alias{backfitRichards-package}
\alias{backfitRichards}
\docType{package}
\title{Backfitted independent values of Richards curves}
\description{
 Backfitted values of the independent variable
 of Richards curves are computed and plotted.
}
\details{
\tabular{ll}{
Package: \tab backfitRichards        \cr
Type:    \tab Package                \cr
Version: \tab 0.4                    \cr
Date:    \tab 2008-03-27             \cr
License: \tab GPL Version 2 or later \cr
}
}
\author{
Jens Henrik Badsberg

Maintainer: Jens Henrik Badsberg <march2008spam4richards@badsberg.eu>
}
% 
% Emmens CW.:
% The dose response relationship for certain principles of the pituitary gland,
% and of the serum and urine of pregnancy.
% Journal of Endocr. 1940; 2:194-225. 
% 
% Reeve R.:
% Two statistical methods for estimating relative potency of bioassays.
% Biopharm Int. 2000 July; 13(7):54-60. 
% 
% Klein J, Capen R, Mancinelli R, Robinett R,
% Pietrobon FPJ, Quinn J, Schofield T.:
% Validation of assays for use with combination vaccines,
% Biologicals 1999; 27:35-41. 
% 
\references{
Richards, F.J.: A flexible growth function for empirical use.
J. Exp. Bot. 1959; 10:290-300.

Finney DJ.: Statistical methods in biological assay,
3rd ed. London: Griffin; 1978. 

Finney DJ.: Response curves for radioimmunoassay.
Clinical Chemistry 1983; 29:1562-1566. 

Schofield T.: Assay Validation.
In: Encyclopedia of Biopharmaceutical Statistics.
New York: Marcel Dekker 2000; p. 21-30. 

\code{http://en.wikipedia.org/wiki/ELISA}
}
\keyword{ package }
\examples{

## Not run: 

library(nlme)

data(ChickWeight)

ChickWeight <- cbind(ChickWeight, ExpTime = exp(ChickWeight[,"Time"]))

groupedChicks <- groupedData(weight ~ ExpTime | Diet / Chick, 
                             inner = ~ PR, data = ChickWeight)

listChicks <- split(groupedChicks, list(groupedChicks[,"Diet"],
                                        groupedChicks[,"Chick"]),
                    drop = TRUE)

fit.Fpl.BP <- function(X, start, lower, upper)
 {
  try(nls(weight ~ SSny1(ExpTime, a, d, b, x50),
          lower = lower, upper = upper, algorithm = "port",
          control = nls.control(warnOnly = TRUE), data = X))
 }

Start <- list(a =    25, d =  200, b =  10, x50 =    10, ny =  1)
Lower <- list(a = -2000, d =    5, b =   0, x50 =     1, ny =  0.01)
Upper <- list(a =    50, d = 1500, b =  50, x50 = 10^20, ny = 128)

myApplyFUN2X <- 
  function(X, FUN, start = Start, lower = Lower, upper = Upper,
              Report = print, ...) {
    if (dim(X)[1] > 0) {
      result <- FUN(X, start, lower, upper)
      if (is.null(result) | class(result) == "try-error")
        Report(X)
      return(result)
    }
  }

fits.Fpl.BP <- fitList(listChicks, FUN = fit.Fpl.BP, n = 4,
                       applyFUN2X = myApplyFUN2X)

back.Fits <- backFits(listChicks, FUN = fit.Fpl.BP, ny = 1,
                      respName = "weight", indepName = "ExpTime",
                      applyFUN2X = myApplyFUN2X)

par(mfrow = c(2, 2))

Z.back <- backPlot(backFits = back.Fits, xt = function(x) x,
                   main = "Relative backfitted exp(time)", xlab = "Exp(Time)", 
                   ylim = c(0, 500), log = "x", pch = 1, col = "red")

Z.back <- backPlot(backFits = back.Fits, xt = function(x) log(x),
                   main = "Relative backfitted exp(time)", xlab = "Time", 
                   ylim = c(0, 500), pch = 1, col = "red")

Z.back <- backPlot(backFits = back.Fits, xt = function(x) log(x),
                   main = "Relative backfitted exp(time)", xlab = "Time", 
                   log = "y", ylim = c(1, 5000), pch = 1, col = "red")

Z.back <- backPlot(backFits = back.Fits, xt = function(x) log(x),
                   main = "Relative backfitted exp(time)", xlab = "Time", 
                   log = "y", ylim = c(10, 500), pch = 1, col = "red")

## End(Not run)

}
