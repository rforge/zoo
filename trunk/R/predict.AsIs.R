
predict.AsIs <- function(obj, data, ...) {
   if (missing(data)) return(fitted(obj))
   # transform y ~ x + ... to x ~ x + ...
   fo <- formula(obj)
   fo[[2]] <- if (length(fo[[3]]) == 1) fo[[3]] else fo[[3]][[2]]
   class(obj) <- class(obj)[-grep("AsIs", class(obj))]
   zoo(predict(obj, data, ...), time(resid(lm(I(fo), data))))
}


