#Z# changes in yearmon: it seemed a bit unclear to me
#Z# whether yearmon should be an extension of the "Date"
#Z# class or an extension of "numeric" vectors. From the docs
#Z# it seemed to be clearly the latter. Hence, my modifications
#Z# are such that
#Z# - underlying scale is always numeric (not Date)
#Z# - scale is always forced to be floor(12*x + .0001)/12
#Z# - I think the unit argument shouldn't be supported, it's equally 
#Z#   simple to do as.numeric(x/12) and as.numeric(x, unit = "month")
#Z# - the default Date is the first of the month

## class creation
yearmon <- function(x) structure(floor(12*x + .0001)/12, class = "yearmon")

## coercion to yearmon: always go via numeric
as.yearmon <- function(x, ...) UseMethod("as.yearmon")
as.yearmon.default <- function(x, ...) as.yearmon(as.numeric(x))
as.yearmon.numeric <- function(x, ...) structure(floor(12*x + .0001)/12, class = "yearmon")
as.yearmon.integer <- function(x, ...) structure(x, class = "yearmon")

## coercion from yearmon
# returned Date is the fraction of the way through the period given by frac
as.Date.yearmon <- function(x, frac = 0, ...) {
     x <- unclass(x)
     year <- floor(x + .001)
     month <- floor(12 * (x - year) + 1 + .5 + .001)
     dd.start <- as.Date(paste(year, month, 1, sep = "-")) 
     dd.end <- dd.start + 32 - as.numeric(format(dd.start + 32, "%d"))
     as.Date((1-frac) * as.numeric(dd.start) + frac * as.numeric(dd.end))
}
as.POSIXct.yearmon <- function(x, tz = "") as.POSIXct(as.Date(x), tz = tz)
as.POSIXlt.yearmon <- function(x, tz = "") as.POSIXlt(as.Date(x), tz = tz)
as.numeric.yearmon <- function(x) unclass(x)
as.character.yearmon <- function(x) format.yearmon(x)

## other methods for class yearmon
c.yearmon <- function(...)
    structure(do.call("c", lapply(list(...), as.numeric)), class = "yearmon")

format.yearmon <- function (x, format = "%b %Y", ...) 
{
    xx <- format(as.Date(x), format = format, ...)
    names(xx) <- names(x)
    xx
}

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

## previous version: 
##
## as.yearmon.yearqtr <- function(x, ...) as.yearmon(as.Date(x))
## as.yearmon.numeric <- function(x, unit = "year", ...) switch(unit,
##	year = structure(x, class = "yearmon"),
##	month = structure(x/12, class = "yearmon"))
## as.yearmon.integer <- function(x, unit = "year", ...) switch(unit,
##	year = structure(x, class = "yearmon"),
##	month = structure(x/12, class = "yearmon"))
## as.yearmon.default <- function(x, ...) 
##  as.yearmon(as.Date(x))
## as.yearmon.yearmon <- function(x, ...) x
##
## MATCH.yearmon <- function(x, table, nomatch = NA, ...)
## 	match(as.Date(x), as.Date(table), nomatch = nomatch, ...)


