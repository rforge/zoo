defaultfrequency <- function(x) UseMethod("defaultfrequency")
defaultfrequency.Date <- function(x) 1
defaultfrequency.yearmon <- function(x) 12
defaultfrequency.default <- function(x) round(1/min(diff(as.numeric(x))))

as.ts.zoo <- function(x, start = as.numeric(time(x[1])), 
		frequency = defaultfrequency(time(x))) {
	tt <- as.integer(round(as.numeric(time(x)) * frequency))
	stopifnot(!any(duplicated(tt)))
	xx <- zoo(coredata(x), tt)
	xx <- merge(xx, zoo(, seq(head(tt,1), tail(tt,1))))
	zz <- ts(coredata(xx), start = start, frequency = frequency)
	zz
}

