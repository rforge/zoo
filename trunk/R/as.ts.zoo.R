defaultfrequency <- function(x, ...) UseMethod("defaultfrequency")
defaultfrequency.Date <- function(x, ...) 1
defaultfrequency.yearmon <- function(x, ...) 12
defaultfrequency.default <- function(x, ts.eps = getOption("ts.eps"), ...) {
	d <- diff(as.numeric(x))
	deltat <- min(d)
	frequency <- 1/deltat
	if (frequency > 1 && abs(frequency - round(frequency)) < ts.eps) 
		frequency <- round(frequency)
	stopifnot(max(abs(frequency*d - round(frequency*d))) < ts.eps)
	frequency
}

as.ts.zoo <- function(x, start = as.numeric(time(x[1])),
   frequency = defaultfrequency(time(x), ts.eps = ts.eps), deltat = 1,
   ts.eps = getOption("ts.eps"), ...) {
	if (missing(deltat)) deltat <- 1/frequency else frequency <- 1/deltat
	round. <- function(x) deltat * round(x/deltat)
	tt <- round.(time(x))
	tt2 <- round.(seq(head(tt,1), tail(tt,1), deltat))
	xx <- merge(zoo(coredata(x), tt), zoo(, tt2))
	ts(coredata(xx), start = start, frequency = frequency)
}

