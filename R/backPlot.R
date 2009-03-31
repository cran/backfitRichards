`backPlot` <-
function (backFits = NULL, indep2conc = function(x) x,
    ylim = c(min(Z), max(Z)),
    Conf = backConf, main = "Relative backfitted concentration", 
    sub = paste("Above / below / NA: ",
                paste("", c(length(which(Z > ylim[2])), 
                            length(which(Z < ylim[1])), 
                            length(which(is.na(Z)))), collapse = " /")), 
    xlab = "Concentration [ Dilution step ]", 
    ylab = "Recovery (%)", 
    doPlot = TRUE, ...) 
{
    RecovPct <- lapply(backFits, function(i) i[, "RecovPct"])
    lengths <- unlist(lapply(RecovPct, length))
    if (min(lengths) == max(lengths)) {
        if (length(RecovPct) > 1) {
            Z <- matrix(unlist(RecovPct), ncol = length(RecovPct))
            dimnames(Z) <- list(names(RecovPct[[1]]), names(RecovPct))
        } else
            Z <- RecovPct[[1]]
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
      if (is.matrix(Z))
        dimnames(Z) <- list(xv, names(RecovPct))
      else { 
        if (!is.null(Z))
          names(Z) <- names(RecovPct) }
    }
    if (doPlot) {
        if (is.matrix(Z)) {
            x <- as.real(dimnames(Z)[[1]])
        } else
            x <- as.real(names(Z))
        suppressWarnings(matplot(indep2conc(x), Z,
            ylim = ylim, main = main, sub = sub, 
            xlab = xlab, ylab = ylab, ...))
        if (is.matrix(Z))
            Conf(Z, x = indep2conc(x), ylim = ylim, ...)
    }
    Z
}
