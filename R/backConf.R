`backConf` <-
function (Z, x = as.real(dimnames(Z)[[1]]), ylim = c(min(Z), max(Z)),
    log = "", lineLty = 2, lineCol = "black", lineLwd = 0.5, centerLty = 2, 
    centerCol = "blue", centerLwd = 1, confCex = 1, confCol = "blue", 
    confLwd = 1.5, ...) 
{
    dx <- ((max(x) - min(x)) * 0.15/20) * confCex
    Dx <- 1
    if ((log == "xy") | (log == "x")) {
        dx <- 0
        Dx <- (1 - 0.5/20) * confCex
    }
    LogZ <- log(Z)
    y <- apply(LogZ, 1, mean, na.rm = TRUE)
    var <- apply(LogZ, 1, function(y) 
                              if (length(y[!is.na(y)]) > 0)
                                  var(y, na.rm = TRUE) 
                              else return(-1))
    n <- apply(LogZ, 1, function(x) length(which(!is.na(x))))
    sd <- 2 * sqrt(var/ifelse(n > 0, n, -0.5))
    sd <- ifelse(is.na(sd) | n < 1, 0, sd)
    lines(range(x), c(100, 100), col = lineCol, lty = lineLty, 
        lwd = lineLwd)
    lines(x, exp(y), col = centerCol, lty = centerLty, lwd = centerLwd)
    segments(c(x, x) * Dx - dx, exp(y + c(sd, -sd)), c(x, x)/Dx + 
        dx, exp(y + c(sd, -sd)), col = confCol, lwd = confLwd)
    segments(x, exp(y - sd), x, exp(y + sd), col = confCol, lwd = confLwd)
    cbind(x = x, y = y, n = n, var = var, sd = sd)
}
