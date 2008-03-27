`backFits` <-
function (list, FUN, ...) 
lapply(list, function(X) backFit(X, FUN, ...))
