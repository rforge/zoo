
as.yearmon <- function(x, ...) UseMethod("as.yearmon")
as.yearmon.yearqtr <- function(x, ...) as.yearmon(as.Date(x))
as.yearmon.numeric <- function(x, unit = "year", ...) switch(unit,
	year = structure(x, class = "yearmon"),
	month = structure(x/12, class = "yearmon"))
as.yearmon.integer <- function(x, unit = "year", ...) switch(unit,
	year = structure(x, class = "yearmon"),
	month = structure(x/12, class = "yearmon"))
as.yearmon.default <- function(x, ...) 
  as.yearmon(as.Date(x))
as.yearmon.yearmon <- function(x, ...) x
as.POSIXct.yearmon <- function(x, tz)
	as.POSIXct(as.Date(x), tz = tz)
as.POSIXlt.yearmon <- function(x, tz = "") 
	as.POSIXlt(as.Date(x), tz = tz)
# returned Date is the fraction of the way through the period given by frac
as.Date.yearmon <- function(x, frac = 1, ...) {
	x <- unclass(x)
	year <- floor(x + .001)
	month <- floor(12 * (x - year) + 1 + .5 + .001)
	dd.start <- as.Date(paste(year, month, 1, sep = "-")) 
	dd.end <- dd.start + 32 - as.numeric(format(dd.start + 32, "%d"))
	as.Date((1-frac) * as.numeric(dd.start) + frac * as.numeric(dd.end))
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
MATCH.yearmon <- function(x, table, nomatch = NA, ...)
	match(as.Date(x), as.Date(table), nomatch = nomatch, ...)
