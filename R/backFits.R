setClass("backFits", contains = "list")

`backFits` <-
function (object, FUN, ...) 
return(new("backFits", lapply(object, function(X) backFit(X, FUN, ...))))
