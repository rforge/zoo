
as.yearmon <- function(x) UseMethod("as.yearmon")
as.yearmon.numeric <- function(x, unit = "year") switch(unit,
	year = structure(x, class = "yearmon"),
	month = structure(x/12, class = "yearmon"))
as.yearmon.integer <- function(x, unit = "year") switch(unit,
	year = structure(x, class = "yearmon"),
	month = structure(x/12, class = "yearmon"))
as.yearmon.default <- function(x) 
  structure( with(as.POSIXlt(x,tz="GMT"), 1900+year+mon/12), 
	class = "yearmon")
as.Date.yearmon <- function(x, ...) {
	x <- unclass(x)
	year <- floor(x + .001)
	month <- floor(12 * (x - year) + 1 + .5 + .001)
	as.Date(paste(year, month, 1, sep = "-"))
}
c.yearmon <- function(...)
   structure(do.call("c", lapply(list(...), as.numeric)), class = "yearmon")
format.yearmon <- function (x, format = "%b %Y", ...) 
{
    xx <- format(as.Date(x), format = format, ...)
    names(xx) <- names(x)
    xx
}
as.character.yearmon <- function(x) format(x)
print.yearmon <- function(x, ...) { 
	print(format(x), ...)
	invisible(x) 
}
"[.yearmon" <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}
axis.yearmon <- function (side, x, at, format, ...) 
	axis.Date(side, as.Date(x), at, format, ...)

