
fitted.AsIs <- function(object, ...) {
   formula <- formula(object)
   env <- environment(formula)
   class(object) <- class(object)[-grep("AsIs", class(object))]
   # .Class <- if (!is.null(attr(object$model, "ts"))) "ts" else
   #	   class(eval(as.list(formula)[[2]], object$model, env))
   #
   # .Class <- attr(attr(z.ts.lm$model, "terms"), "dataClasses")[1]
   .Class <- if (is.null(attr(object$model, "ts"))) class(object) else "ts"
   NextMethod("fitted")
}

fitted.ts <- function(object, ...) {
   tt <- time(do.call("ts.intersect", attr(object$model, "ts")))
   ts(object$fitted, start = start(tt), frequency = frequency(tt))
}

fitted.zoo <- function(object, ...) {
   tt <- time(do.call("ts.intersect", attr(object$model, "ts")))
   as.zoo(object$fitted, frequency = frequency(tt))
}

residuals.AsIs <- function(object, ...) {
   formula <- formula(object)
   env <- environment(formula)
   class(object) <- class(object)[-grep("AsIs", class(object))]
   .Class <- if (is.null(attr(object$model, "ts"))) class(object) else "ts"
   # .Class <- attr(attr(z.ts.lm$model, "terms"), "dataClasses")[1]
   NextMethod("resid")
}

residuals.ts <- function(object, ...) {
   tt <- time(do.call("ts.intersect", attr(object$model, "ts")))
   ts(object$residuals, start = start(tt), frequency = frequency(tt))
}

head.ts <- function(x, n = 6, ...) 
  ts(head(unclass(x), n), start = start(x), frequency = frequency(x))

tail.ts <- function(x, n = 6, ...) 
  ts(tail(unclass(x), n), end = end(x), frequency = frequency(x))
   
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

predict.AsIs <- function(object, newdata, ...) {
   if (missing(newdata)) return(fitted(object))
   formula <- formula(object)
   if (!is.list(newdata)) newdata <- as.list(newdata)
   .Class <- class(eval(as.list(formula)[[2]], newdata, environment(formula)))
   NextMethod("predict")
}

predict.zoo <- function(object, newdata, ...) {
   if (missing(newdata)) return(fitted(object))
   if (!is.list(newdata)) newdata <- as.list(newdata)
   formula <- formula(object)
   class(object) <- class(object)[-grep("AsIs", class(object))]
   # term list except for response
   tl <- as.list(attr(terms(object), "variables")[-1])[-1]
   # argument to time is model matrix as a zoo object
   tt.pred <- eval(as.call(append(merge, tl)), newdata, environment(formula))
   z <- zoo(predict(object, newdata, ...), time(tt.pred), frequency(tt.pred))
}

# workaround for bugs in stats
ts.intersect.2 <- function(...) {
	args <- list(...)
	stopifnot(all(sapply(args, frequency) == frequency(args[[1]])))
	args.zoo <- lapply(args, as.zoo)
	args.zoo$all <- FALSE
	ans <- do.call(merge.zoo, args.zoo)
	ts(coredata(ans), start = start(ans), frequency = frequency(args[[1]]))
}

predict.ts <- function(object, newdata, ...) {
   if (missing(newdata)) return(fitted(object))
   if (!is.list(newdata)) newdata <- as.list(newdata)
   formula <- formula(object)
   class(object) <- class(object)[-grep("AsIs", class(object))]
   env <- environment(formula)
   # term list
   tl.pred <- as.list(attr(terms(object), "variables")[-1])[-1]
   # tt.pred <- eval(as.call(append(ts.intersect, tl.pred)), newdata, env)
   tt.pred <- eval(as.call(append(ts.intersect.2, tl.pred)), newdata, env)
   # tt.pred <- do.call(ts.intersect, lapply(tt.pred, na.omit))
   # term list, response only
   # tl.resp <- as.list(attr(terms(object), "variables")[-1])[1]
   # tt.resp <- eval(as.call(append(ts.intersect, tl.pred)), newdata, env)
   # replace "ts" with "other" in dataClasses attributes of term component
   dc <- attr(object$terms, "dataClasses")
   idx <- grep("numeric", dc)
   attr(object$terms, "dataClasses")[idx] <- "other"
   z <- ts(predict(object, newdata, ...), start = start(tt.pred), 
	frequency = frequency(tt.pred))
}


