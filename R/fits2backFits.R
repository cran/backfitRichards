`fits2backFits` <-
function (list, fits, ...) 
{
    n <- length(list)
    if (!(n == length(fits))) 
        stop("Mismatch: Lists not of same length")
    if (!all(names(list) == names(fits))) 
        warning("Possible mismatch")
    result <- NULL
    for (i in 1:n) result <- append(result, list(backFit(list[[i]], 
        fit = fits[[i]], ...)))
    names(result) <- names(list)
    result
}
