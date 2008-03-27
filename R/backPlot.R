`backPlot` <-
function (backFits = NULL, xt = function(x) 1/x, ylim = c(min(Z), max(Z)),
    Conf = backConf, main = "Relative backfitted concentration", 
    sub = paste("Above / below / NA: ", paste("", c(length(which(Z > 
        ylim[2])), length(which(Z < ylim[1])), length(which(is.na(Z)))), 
        collapse = " /")), xlab = "Concentration [ Dilution step ]", 
    ylab = "Recovery (%)", ...) 
{
    RecovPct <- lapply(backFits, function(i) i[, "RecovPct"])
    lengths <- unlist(lapply(RecovPct, length))
    if (min(lengths) == max(lengths)) {
      Z <- matrix(unlist(RecovPct), ncol = length(RecovPct))
      dimnames(Z) <- list(names(RecovPct[[1]]), names(RecovPct))
    } else {
      xv <- unique(sort(unlist(lapply(backFits, function(i) i[, "x"]))))
      A <- xv
      lapply(backFits, function(i) {
                         idx <- match(i[, "x"], xv)
                         row <- rep(NA, length(xv))
                         row[idx] <- i[, "RecovPct"]
                         A <<- cbind(A, row)
                       })
      Z <- A[, -1]
      dimnames(Z) <- list(xv, names(RecovPct))
    }
    suppressWarnings(matplot(xt(as.real(dimnames(Z)[[1]])), Z,
        ylim = ylim, main = main, sub = sub, 
        xlab = xlab, ylab = ylab, ...))
    Conf(Z, x = xt(as.real(dimnames(Z)[[1]])), ylim = ylim, ...)
    Z
}
