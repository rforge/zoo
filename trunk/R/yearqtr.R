
as.yearqtr <- function(x, ...) UseMethod("as.yearqtr")
as.yearqtr.yearmon <- function(x, ...) as.yearqtr(as.Date(x))
as.yearqtr.numeric <- function(x, unit = "year", ...) switch(unit,
	year = structure(floor(4*x+.0001) / 4, class = "yearqtr"),
	quarter = structure((x+.0001)%/%4, class = "yearqtr"))
as.yearqtr.integer <- function(x, unit = "year", ...) switch(unit,
	year = structure(x, class = "yearqtr"),
	month = structure(x/4, class = "yearqtr"))
as.yearqtr.default <- function(x, ...) 
  as.yearqtr(as.Date(x))
as.yearqtr.yearqtr <- function(x, ...) x
# frac is fraction of the way through the period to set the Date
as.POSIXct.yearqtr <- function(x, tz= "")
	as.POSIXct(as.Date(x, frac = 1), tz = tz)
as.POSIXlt.yearqtr <- function(x, tz = "")
	as.POSIXlt(as.Date(x), tz = tz)
as.Date.yearqtr <- function(x, frac = 1, ...) {
	x <- unclass(x)
	year <- floor(x + .001)
	month <- floor(12 * (x - year) + 1 + .5 + .001)
	dd.start <- as.Date(paste(year, month, 1, sep = "-"))
	dd.end <- dd.start + 100 - as.numeric(format(dd.start + 100, "%d")) 
	as.Date((1-frac) * as.numeric(dd.start) + frac * as.numeric(dd.end))
}
c.yearqtr <- function(...)
   structure(do.call("c", lapply(list(...), as.numeric)), class = "yearqtr")
format.yearqtr <- function (x, format = "%b %Y", ...) 
{
	x <- unclass(x)
	year <- floor(x + .001)
	qtr <- floor(4*(x - year) + 1 + .5 + .001)
	xx <- paste(year, "Q", qtr, sep = "")
	names(xx) <- names(x)
	xx
}
as.character.yearqtr <- function(x) format(x)
print.yearqtr <- function(x, ...) { 
	print(format(x), ...)
	invisible(x) 
}
"[.yearqtr" <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}
axis.yearqtr <- function (side, x, at, format, ...) 
	axis.Date(side, as.Date(x), at, format, ...)

MATCH.yearqtr <- function(x, table, nomatch = NA, ...)
	match(as.Date(x), as.Date(table), nomatch = nomatch, ...)
