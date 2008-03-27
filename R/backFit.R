`backFit` <-
function (X, FUN = NULL, fit = NULL, ny = 0,
    respName = "SIGNAL", indepName = "ARGX", 
    applyFUN2X = function(X, FUN, ...) FUN(X, ...), ...) 
{
    if (is.null(fit)) 
        fit <- applyFUN2X(X, FUN, ...)
    SIGNAL <- lapply(split(X[, respName], X[, indepName]), mean)
    Signal <- unlist(SIGNAL)
    Argx <- as.real(names(SIGNAL))
    Fit <- rep(NA, length(Argx))
    if (is.null(fit) | class(fit) == "try-error") {
        warning("No fit: try-error!")
        cbind(x = Argx, y = Signal, Fit = Fit, BackFit = Fit, 
            RecovPct = Fit)
    }
    else {
        estimates <- summary(fit)$parameters[, "Estimate"]
        .a <- estimates["a"]
        .d <- estimates["d"]
        .b <- estimates["b"]
        .x50 <- estimates["x50"]
        .ny <- estimates["ny"]
        if (is.na(.ny)) 
            .ny <- ny
        Fit <- richards(Argx, 
                        a = .a, d = .d, x50 = .x50, b = .b, ny = .ny)
        BackFit <- richards.inv(Signal, 
                        a = .a, d = .d, x50 = .x50, b = .b, ny = .ny)
        cbind(x = Argx, y = Signal, Fit = Fit, BackFit = BackFit, 
            RecovPct = 100 * (BackFit/Argx))
    }
}
