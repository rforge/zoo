zoo <- function(x = NA, order.by = index(x))
{
  index <- order(order.by)
  order.by <- order.by[index]

  if(is.vector(x))
    x <- rep(x, length.out = length(index))[index]
  else if(is.matrix(x))
    x <- (x[rep(1:NROW(x), length.out = length(index)), , drop = FALSE])[index, , drop = FALSE]
  else if(is.data.frame(x))
    x <- (x[rep(1:NROW(x), length.out = length(index)), , drop = FALSE])[index, , drop = FALSE]  
  else
    stop(paste(dQuote("x"), "has to be a vector or matrix"))

  attr(x, "index") <- order.by
  class(x) <- "zoo"
  return(x)
}

print.zoo <- function(x, 
	style = ifelse(length(dim(x)) == 0, "horizontal", "vertical"), 
	quote = FALSE, ...) 
{
	style <- match.arg(style, c("horizontal", "vertical", "plain"))
	if (length(dim(x)) > 0 && style == "horizontal") style <- "plain"
	if (style == "vertical") {
		y <- format(as.matrix(x))
		if (length(colnames(x)) < 1) {
			lab <- deparse(substitute(x))
			colnames(y) <- if (NCOL(x) == 1) lab
			  else paste(lab, 1:NCOL(x), sep=".")
		}
		rownames(y) <- as.character(index(x))
		print(y, quote = quote, ...)
	} else if (style == "horizontal") {
		y <- as.vector(x)
		names(y) <- as.character(index(x))
		print(y, quote = quote, ...)
	} else {
		x.index <- index(x)
		attr(x,"index") <- NULL
		cat("Value:\n")
		print(unclass(x))
		cat("\nIndex:\n")
		print(x.index)
	}
	invisible(x)
}

summary.zoo <- function(x, ...) 
{
	y <- as.data.frame(x)
	if (length(colnames(x)) < 1) {
		lab <- deparse(substitute(x))
		colnames(y) <- if (NCOL(x) == 1) lab
		  else paste(lab, 1:NCOL(x), sep=".")
	}
	summary(cbind(data.frame(Index = index(x)), y), ...)
}


is.zoo <- function(object)
  inherits(object, "zoo")

str.zoo <- function(object, ...)
{
  str(unclass(object), ...)
}

"[.zoo" <- function(x, i, j, drop = TRUE, ...)
{
  if(!is.zoo(x)) stop("method is only for zoo objects")
  x.index <- index(x)
  attr(x, "index") <- NULL
  nclass <- class(x)[-(1:which(class(x) == "zoo"))]
  if(length(nclass) < 1) nclass <- NULL 
  class(x) <- nclass
  if(missing(i)) i <- 1:NROW(x)
  if(length(dim(x)) == 2) {
        if(missing(j)) j <- 1:ncol(x)
	zoo(x[i, j, drop = drop, ...], x.index[i])
   } else
	zoo(x[i], x.index[i])
}

window.zoo <- function(x, index = index.zoo(x), start = NULL, end = NULL, ...)
{
  all.indexes <- index.zoo(x)
  in.index <- all.indexes %in% index

  if(is.null(start)) {
    if(is.null(end)) {
      wi <- which(all.indexes %in% index)
      return(x[wi,,])
    } else {
      wi <- which(in.index & all.indexes <= end)
      return(x[wi,,])
    }
  } else {
    if(is.null(end)) {
      wi <- which(in.index & all.indexes >= start)
    } else {
      wi <- which(in.index & all.indexes >= start & all.indexes <= end)
    }
    return(x[wi,,])
  }
}

"window<-" <- function(x, value, ...) 
{
	UseMethod("window<-")
}

"window<-.zoo" <- function(x, index = index.zoo(x), start = NULL, end = NULL, value)
{
	ix <- index.zoo(x)
	stopifnot(all(index %in% ix))
	if (!is.null(start)) index <- index[index >= start]
	if (!is.null(end)) index <- index[index <= end]

	wi <- which(ix %in% index)
	if (length(dim(x)) == 0)
		x[wi] <- value
	else
		x[wi,] <- value
	return(x)
}
 
head.zoo <- function(x, n = 6, ...) {
	if (length(dim(x)) == 0)
		x[seq(length = min(n, length(x)))]
	else
		x[seq(length = min(n, nrow(x))),]
}
 
tail.zoo <- function(x, n = 6, ...) {
	if (length(dim(x)) == 0)
		x[seq(to = length(x), length = min(n, length(x)))]
	else
		x[seq(to = nrow(x), length = min(n, nrow(x))),]
}



## currently needed for making zoo independent of
## index class
order <- function(x, ...)
	UseMethod("order")

order.default <- function(x, ...) {
	base::order(x, ...)
}


match <- function(x, y, ...)
	UseMethod("match")
	
match.default <- function(x, y, ...)
	base::match(x, y, ...)

