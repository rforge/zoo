
predict.AsIs <- function(object, newdata, ...) {
   if (missing(newdata)) return(fitted(object))
   class(object) <- class(object)[-grep("AsIs", class(object))]
   zoo(predict(object, newdata, ...), time(LAST.SERIES))
}

