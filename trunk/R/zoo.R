zoo <- function (x, order.by = index(x)) 
{
    if (missing(order.by)) order.by = index(x)
    index <- ORDER(order.by)
    order.by <- order.by[index]
    if (missing(x) || is.null(x)) 
        x <- numeric()
    else if (is.vector(x) || is.factor(x)) 
        x <- rep(x, length.out = length(index))[index]
    else if (is.matrix(x) || is.data.frame(x)) 
        x <- (x[rep(1:NROW(x), length.out = length(index)), , 
            drop = FALSE])[index, , drop = FALSE]
    else if (is.data.frame(x)) 
        x <- (x[rep(1:NROW(x), length.out = length(index)), , 
            drop = FALSE])[index, , drop = FALSE]
    else stop(paste(dQuote("x"), "has to be a vector or matrix"))
    attr(x, "index") <- order.by
    class(x) <- c("zoo", class(x))
    x
}

print.zoo <-
function (x, style = ifelse(length(dim(x)) == 0, "horizontal", 
    "vertical"), quote = FALSE, ...) 
{
    style <- match.arg(style, c("horizontal", "vertical", "plain"))
    if (is.null(dim(x)) || (length(dim(x)) > 0 && style == "horizontal"))
	style <- "plain"
    if (style == "vertical") {
        y <- format(eval(as.matrix(x), parent.frame(n = 3)))
        if (length(colnames(x)) < 1) {
            colnames(y) <- rep("", NCOL(x))
        }
        rownames(y) <- as.character(index(x))
        print(y, quote = quote, ...)
    }
    else if (style == "horizontal") {
        y <- as.vector(x)
        names(y) <- as.character(index(x))
        print(y, quote = quote, ...)
    }
    else {
        x.index <- index(x)
        attr(x, "index") <- NULL
        cat("Value:\n")
        print(unclass(x))
        cat("\nIndex:\n")
        print(x.index)
    }
    invisible(x)
}

summary.zoo <- function(object, ...) 
{
	y <- as.data.frame(object)
	if (length(colnames(object)) < 1) {
		lab <- deparse(substitute(object))
		colnames(y) <- if (NCOL(object) == 1) lab
		  else paste(lab, 1:NCOL(object), sep=".")
	}
	summary(cbind(data.frame(Index = index(object)), y), ...)
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




as.Date.numeric <- function(x, ...)
	structure(floor(x + .001), class = "Date")

as.Date.integer <- function(x, ...)
	structure(x, class = "Date")

as.Date.ts <- function(x, ...) {
   # if by is "days" then times of x are treated as days since as.Date(0);
   # otherwise, they are treated as last two digits of years if 1st time < 100
   # or years if 1st time > 100.  If 1st time < yearcutoff then century is 
   # assumed to be 2000; otherwise, it is assumed to be 1900. Default for
   # yearcutoff is 25.
   args <- list(...)
   yearcutoff <- if (is.null(args$yearcutoff)) 25 else args$yearcutoff
   by <- if (is.null(args$by)) 1 else pmatch(args$by, c("years", "days"))
   time.x <- unclass(time(x))
   if (by == 2 ) {
	stopifnot(frequency(x) == 1)
	return(as.Date(time.x))
   }
   if (time.x[1] <= yearcutoff) 
	time.x <- time.x + 2000
   else if (time.x[1] < 100)
	time.x <- time.x + 1900
   if (frequency(x) == 1)
	as.Date(paste(time.x, 1, 1, sep = "-"))
   else if (frequency(x) == 4)
	as.Date(paste((time.x + .001) %/% 1, 3*(cycle(x)-1)+1, 1, sep = "-"))
   else if (frequency(x) == 12)
	as.Date(paste((time.x + .001) %/% 1, cycle(x), 1, sep = "-"))
   else
	stop("unable to convert ts time to Date class")
}

