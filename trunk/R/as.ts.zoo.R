defaultfrequency <- function(x, ...) UseMethod("defaultfrequency")
defaultfrequency.Date <- function(x) 1
defaultfrequency.yearmon <- function(x) 12
defaultfrequency.default <- function(x, ts.eps = getOption("ts.eps")) {
	d <- diff(as.numeric(x))
	deltat <- min(d)
	frequency <- 1/deltat
	if (frequency > 1 && abs(frequency - round(frequency)) < ts.eps) 
		frequency <- round(frequency)
	stopifnot(max(abs(frequency*d - round(frequency*d))) < ts.eps)
	frequency
}

as.ts.zoo <- function(x) {
	# next two lines should become args if as.ts in R 2.1.0
	# is changed from function(x) to function(x, ...)
	start = as.numeric(time(x[1])) 
	frequency = defaultfrequency(time(x), ts.eps = getOption("ts.eps"))
	deltat <- 1/frequency
	tt <- as.numeric(time(x))
	xx <- zoo(coredata(x), tt)
	xx <- merge(xx, zoo(, seq(head(tt,1), tail(tt,1), deltat)))
	zz <- ts(coredata(xx), start = start, frequency = frequency)
	zz
}

