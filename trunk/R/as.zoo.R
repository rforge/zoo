as.zoo <- function(x, frequency, ...)
{
  UseMethod("as.zoo")
}

as.zoo.default <- function(x, frequency = NULL, ...)
{
  if(is.zoo(x)) x
    else zoo(structure(x, dim = dim(x)), index(x), frequency = frequency)
}

as.zoo.factor <- function(x, frequency = NULL, ...) 
{
  L <- list(...)
  stopifnot(length(L) < 2)
  if (length(L))
	zoo(x, list(...)[[1]], frequency = frequency)
  else
	zoo(x, frequency = frequency)
}

as.zoo.ts <- function(x, frequency = stats::frequency(x), timeclass, ... )
{
  if (missing(timeclass)) {
	timeclass <- if (frequency(x) == 12) "yearmon"
		else if (frequency(x) == 4) "yearqtr"
		else "numeric"
  }
  tt <- unclass(time(x))
  tt <- eval(parse(text=paste("as", timeclass, sep = ".")))(tt)
  zoo(coredata(x), tt, frequency = frequency)
} 


as.zoo.irts <- function(x, frequency = NULL, ...)
{
  zoo(x$value, x$time)
}

as.zoo.its <- function(x, frequency = NULL, ...) 
{
	index <- attr(x, "dates")
	class(x) <- attr(x, "dates") <- NULL
	zoo(x, index)
}

as.zoo.zoo <- function(x, frequency, ...)
	if (missing(frequency)) x else {
		attr(x, "frequency") <- frequency
		x
	}

as.ts.zoo <- function(x, start = as.numeric(time(x[1])),
   frequency = frequency(x), deltat, ts.eps = getOption("ts.eps"), ...) {
	stopifnot(!is.null(frequency))
	if (missing(deltat)) deltat <- 1/frequency else frequency <- 1/deltat
	round. <- function(x) deltat * round(x/deltat)
	tt <- round.(as.numeric(time(x)))
	tt2 <- round.(seq(head(tt,1), tail(tt,1), deltat))
	xx <- merge(zoo(coredata(x), tt), zoo(, tt2))
	ts(coredata(xx), start = start, frequency = frequency)
}

as.its.zoo <- function(x) {
	stopifnot(require(its))
	index <- attr(x, "index")
	stopifnot(inherits(index, "POSIXct"))
	attr(x, "index") <- NULL
	its(unclass(x), index)
}


as.vector.zoo <- function(x, mode = "any")
	as.vector(as.matrix(x), mode = mode)

as.matrix.zoo <- function (x) 
{
    y <- as.matrix(unclass(x))
    attr(y, "index") <- NULL
    if (length(y) > 0) 
	    colnames(y) <- if (length(colnames(x)) > 0) 
		colnames(x)
	    else {
		lab <- deparse(substitute(x))
		if (NCOL(x) == 1) 
		    lab
		else paste(lab, 1:NCOL(x), sep = ".")
	    }
    return(y)
}

as.data.frame.zoo <- function(x, row.names = NULL, optional = FALSE)
{
	y <- as.data.frame(unclass(x))
        if(NCOL(x) > 0) {
		colnames(y) <- if (length(colnames(x)) > 0) 
			colnames(x)
		else {
			lab <- deparse(substitute(x))
			if (NCOL(x) == 1) lab
	                  else paste(lab, 1:NCOL(x), sep = ".")
		}
	}
	if (!is.null(row.names)) row.names(y) <- row.names
	return(y)
}

as.list.zoo <- function(x, ...) {
	if (length(dim(x)) == 0) list(x)
  		else lapply(as.data.frame(x), zoo, index(x), frequency(x))
}

as.list.ts <- function(x, ...) {
	if (is.matrix(x))
		lapply(as.data.frame(x), ts, 
			start = start(x), end = end(x), freq = frequency(x))
	else
		list(x)
}

