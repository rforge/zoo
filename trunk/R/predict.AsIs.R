
predict.AsIs <- function(object, newdata, ...) {
   if (missing(newdata)) return(fitted(object))
   class(object) <- class(object)[-grep("AsIs", class(object))]
   # term list except for response
   tl <- as.list(attr(terms(object), "variables")[-1])[-1]
   env <- environment(formula(object))
   # argument to time is model matrix as a zoo object
   z <- zoo(predict(object, newdata, ...), 
	   time(eval(as.call(append(merge.zoo, tl)), newdata, env)))
}

