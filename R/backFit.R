setClass("backFit", contains = "matrix")

`backFit` <-
function (X, FUN = NULL, fit = NULL, ny = 0,
    respName = "SIGNAL", indepName = "ARGX", 
    applyFUN2X = function(X, FUN, ...) FUN(X, ...), ...) 
{
    P <- function(estimates) {
        .a <- estimates["a"]
        .d <- estimates["d"]
        if (is.element("scal", names(estimates)))
          .b <- 1 / estimates["scal"]
        else
          .b <- estimates["b"]
        if (is.element("g", names(estimates)))
          .ny <- 1 / estimates["g"]
        else
          .ny <- estimates["ny"]
        if (is.na(.ny)) 
            .ny <- ny
        if (is.element("scal", names(estimates)))
          .x50 <- exp(estimates["xmid"])
        else if (is.element("e", names(estimates)))
          .x50 <- exp(estimates["e"]) * ((2^(.ny) - 1)/.ny)^(1/.b)
        else
          .x50 <- estimates["x50"]
        return(list(a = .a, d = .d, b = .b, x50 = .x50, ny = .ny))
    }
    Richards <- function(x, P)
        richards(x, a = P$a, d = P$d, x50 = P$x50, b = P$b, ny = P$ny)
    Richards.inv <- function(y, P)
        richards.inv(y, a = P$a, d = P$d, x50 = P$x50, b = P$b, ny = P$ny)
    invalidFit <- function(fit)
        is.null(fit) | any(class(fit) == c("try-error", "data-error"))

    if (is.null(fit)) 
        fit <- applyFUN2X(X, FUN, ...)
    if (length(which(dimnames(X)[[2]] == respName)) != 1)
        warning("Invalid 'respName' in 'backFit'")
    if (length(which(dimnames(X)[[2]] == indepName)) != 1)
        warning("Invalid 'indepName' in 'backFit'")
    SIGNAL <- lapply(split(X[, respName], X[, indepName]), mean, na.rm = TRUE)
    Signal <- unlist(SIGNAL)
    Argx <- as.real(names(SIGNAL))
    Fit <- rep(NA, length(Argx))
    if (invalidFit(fit)) {
        message(paste("No fit (backFit): ", class(fit), "!"))
        result <- cbind(x = Argx, y = Signal, Fit = Fit, BackFit = Fit, 
            RecovPct = Fit)
    }
    else {
        estimates <- P(summary(fit)$parameters[, "Estimate"])
        Fit <- Richards(Argx, estimates)
        BackFit <- Richards.inv(Signal, estimates) 
        result <- cbind(x = Argx, y = Signal, Fit = Fit, BackFit = BackFit, 
            RecovPct = 100 * (BackFit/Argx))
    }
    return(new("backFit", result))
}
