\name{backPlot}
\alias{backPlot}
\title{ Plot backfitted values for Richards curves }
\description{ Backfitted values for Richards curves are plotted.}
\usage{
backPlot(backFits = NULL, indep2conc = function(x) x,
         ylim = c(min(Z), max(Z)),
         Conf = backConf, main = "Relative backfitted concentration",
         sub = paste("Above / below / NA: ",
                     paste("", c(length(which(Z > ylim[2])),
                                 length(which(Z < ylim[1])),
                                 length(which(is.na(Z)))),
                           collapse = " /")),
         xlab = "Concentration [ Dilution step ]",
         ylab = "Recovery (\%)", 
         doPlot = TRUE, ...)
}
\arguments{
  \item{backFits}{ a list of \code{backFits}, see \link{backFit}. }
  \item{ylim}{ a numeric vector with the range of the y axis. }
  \item{indep2conc}{ a function for transforming the x axis. }
  \item{Conf}{ a function for adding confidence intervals,
                default \link{backConf}. }
  \item{main}{ an overall title for the plot: see \link{title}. }
  \item{sub}{ a sub title for the plot: see \link{title}. }
  \item{xlab}{ a title for the x axis: see \link{title}. }
  \item{ylab}{ a title for the y axis: see \link{title}. }
  \item{doPlot}{ a boolean: if FALSE then no plot is made. }
  \item{\dots}{ additional arguments, to \link{matplot} and \code{Conf},
                see \link{matplot} and \link{backConf}. }
}
\details{ backfitted values for Richards curves are plottet.}
\value{ Matrix with the plottet values. }
\author{ Jens Henrik Badsberg }
\keyword{ package }
