length.timeDate <- function(x) prod(x@Dim)
ORDER.timeDate <- function(x, ...) order(as.POSIXct(x), ...)
MATCH.timeDate <- function(x, y, ...) 
	match(as.POSIXct(x), as.POSIXct(y), ...)
as.zoo.timeSeries <- function(x) {
	require(zoo)
	zoo(x@Data, timeDate(x@positions, x@format, x@FinCenter),
		comment = c(title = x@title, documentation = x@documentation))
}
#####################################################################
# delete following line and redefine the current as.timeSeries of fBasic to be
# as.timeSeries.default
#####################################################################
as.timeSeries.default <- fBasic::as.timeSeries
as.timeSeries <- function(x, ...) UseMethod("as.timeSeries")
# as.timeSeries.default <- function(x, ...) fBasics::as.timeSeries(x, ...)
as.timeSeries.zoo <- function(x, tz = "GMT", title = if (length(comment(x)) > 0) comment(x)[1], documentation = if (length(comment(x)) > 1) comment(x)[-1])
{
	require(zoo)
	tt <- time(x)
	stopifnot(inherits(tt, c("timeDate", "POSIXt", "Date")))
	if (inherits(tt, "timeDate")) {
   	   timeSeries(Data(x), format(tt), colnames(x), tt@format, 
		tz, tt@FinCenter, title, documentation)
	} else 
	   timeSeries(Data(x), format(tt), colnames(x), zone = tz,
		title = title, documentation = documentation)
}

"[.sdate" <- function(x, i) structure(unclass(x)[i], class = "sdate")
c.sdate <- function(x, ..., recursive = FALSE) {
	args <- append(list(x), list(...))
	structure(do.call("c", lapply(args, unclass)), class = "sdate")
}
as.sdate <- function(x, ...) UseMethod("as.sdate")
as.sdate.default <- function(x, origin = 19600101) sdate(x, origin = origin)
as.sdate.Date <- function(x) sdate(unclass(x) + 3653)
as.sdate.dates <- function(x) sdate(unclass(x) + 3653)
as.sdate.POSIXt <- function(x, tz = "GMT") as.sdate.Date(as.Date(x, tz = tz))

as.chron.sdate <- function(x) { require(chron); chron(unclass(as.Date(x))) }
as.Date.sdate <- function(x) as.Date(format(x), "%Y%m%d")
as.POSIXct.sdate <- function(x, tz = "GMT") as.POSIXct(as.Date.sdate(x), tz = "GMT")
as.POSIXlt.sdate <- function(x, tz = "GMT") as.POSIXlt(as.Date.sdate(x), tz = "GMT")

as.timeDate <- function(x) UseMethod("as.timeDate")
as.timeDate.sdate <- function(x) timeDate(x)
as.timeDate.default <- function(x) as.timeDate(as.sdate(x))

as.Date.timeDate <- function(x) as.Date(as.sdate(x))
# as.POSIXlt.timeDate <- function(x, tz = "GMT") x@Data
as.POSIXct.timeDate <- function(x,tz = "GMT") as.POSIXct(x@Data, tz = tz)
as.sdate.timeDate <- function(x) as.sdate.Date(as.Date(x@Data))

"-.sdate" <- function (e1, e2) 
{
    coerceTimeUnit <- function(x) {
        round(switch(attr(x, "units"), secs = x/86400, mins = x/1440, 
            hours = x/24, days = x, weeks = 7 * x))
    }
    if (!inherits(e1, "sdate")) 
        stop("Can only subtract from sdate objects")
    if (nargs() == 1) 
        stop("unary - is not defined for sdate objects")
    if (inherits(e2, "sdate")) 
        return(difftime(e1, e2, units = "days"))
    if (inherits(e2, "difftime")) 
        e2 <- unclass(coerceTimeUnit(e2))
    if (!is.null(attr(e2, "class"))) 
        stop("can only subtract numbers from sdate objects")
    structure(unclass(as.Date(e1)) - e2, class = "sdate")
}



### tests ###

require(fBasics)

# test as.timeSeries.zoo and as.zoo.timeSeries

zD <- zoo(1:3, structure(11:13, class = "Date"))
zD
zD. <- as.timeSeries(zD)
zD.
as.zoo(zD.)
merge(zD, zD)

zct <- zoo(1:3, as.POSIXct(time(zD)))
zct
zct. <- as.timeSeries(zct)
zct.
as.zoo(zct.)
merge(zct, zct)

ztd <- zoo(1:3, timeDate(time(zct)))
ztd
ztd. <- as.timeSeries(ztd)
ztd.
as.zoo(ztd.)
merge(ztd, ztd)

# test length, match and order with timeDate class

length(time(ztd))
MATCH(time(ztd)[2], time(ztd))
ORDER(time(ztd))

# test c.sdate and [.sdate

sdatevec <- c(sdate(1:2), sdate(2:3))
sdatevec
sdatevec[2:3]
class(sdatevec[2:3])

