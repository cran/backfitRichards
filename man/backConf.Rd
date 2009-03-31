\name{backConf}
\alias{backConf}
\title{ Pointwise confidence intervals for 'backPlot' }
\description{ Pointwise confidence intervals for 'backPlot'. }
\usage{
backConf(Z, x = as.real(dimnames(Z)[[1]]), ylim = c(min(Z), max(Z)),
         log = "", lineLty = 2, lineCol = "black", lineLwd = 0.5,
         centerLty = 2, centerCol = "blue", centerLwd = 1,
         confCex = 1, confCol = "blue", confLwd = 1.5, ...)
}
\arguments{
  \item{Z}{ a matrix with dependent values. }
  \item{ylim}{ a numeric vector with the range of the y axis. }
  \item{x}{ a numeric vector with the independent values. }
  \item{log}{ the argument \code{log} to \link{matplot}. }
  \item{lineLty}{ a numeric value, \code{lty} for the line y = 100 \%. }
  \item{lineCol}{ a text string, \code{col} for the line y = 100 \%. }
  \item{lineLwd}{ a numeric value, \code{lwd} for the line y = 100 \%. }
  \item{centerLty}{ a numeric value, \code{lty} for joining
                    the means of the dependent values. }
  \item{centerCol}{ a text string, \code{col} for joining
                    the means of the dependent values. }
  \item{centerLwd}{ a numeric value, \code{lwd} for joining
                    the means of the dependent values. }
  \item{confCex}{ a numeric value, \code{cex} for the size
                  of the 'square' of the confidence intervals. }
  \item{confCol}{ a text string, \code{col} of the confidence intervals. }
  \item{confLwd}{ a numeric value, \code{lwd} of the confidence intervals. }
  \item{\dots}{ additional arguments. }
}
\details{ Pointwise confidence intervals for 'backPlot'. }
\author{ Jens Henrik Badsberg }
\keyword{ package }
