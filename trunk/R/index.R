index <- function(x, ...)
{
  UseMethod("index")
}

index.default <- function(x, ...)
{
  seq(length = NROW(x))
}

index.zoo <- function(x, ...)
{
  attr(x, "index")
}

time.zoo <- function(x, ...)
{
  index(x)
}

"index<-" <- function(x, value) 
{
	UseMethod("index<-")
}

"time<-" <- function(x, value) 
{
	UseMethod("time<-")
}

"index<-.zoo" <- function(x, value) 
{
	if(length(index(x)) != length(value)) 
	  stop("length of index vectors does not match")
	attr(x, "index") <- value
	return(x)
}

"time<-.zoo" <- function(x, value) 
{
	if(length(index(x)) != length(value)) 
	  stop("length of time vectors does not match")
	attr(x, "index") <- value
	return(x)
}

start.zoo <- function(x, ...) 
{
	if (length(index(x)) > 0) index(x)[1]
	  else NULL
}

end.zoo <- function(x, ...) 
{
	lx <- length(index(x))
	if (lx > 0) index(x)[lx]
	  else NULL
}

if(!exists("value") || !is.function(get("value"))) {
  ## value is also a generic function (with the same
  ## definition) in tseries  
  value <- function(x, ...) UseMethod("value")
}

value.default <- function(x, ...)
{
	y <- x
	attributes(y) <- NULL
	dim(y) <- dim(x)
	dimnames(y) <- dimnames(x)
	y
}

"value<-" <- function(x, value)
{
	UseMethod("value<-")
}

"value<-.default" <- function(x, value)
{
	stopifnot(length(x) == length(value))
	x[] <- value
	x
}

