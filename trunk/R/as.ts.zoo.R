defaultfrequency <- function(x) UseMethod("defaultfrequency")
defaultfrequency.Date <- function(x) 1
defaultfrequency.yearmon <- function(x) 12
defaultfrequency.default <- function(x) round(1/min(diff(as.numeric(x))))

as.ts.zoo <- function(x, start = as.numeric(time(x[1])), 
		frequency = defaultfrequency(time(x))) {
	tt <- as.integer(round(as.numeric(time(x)) * frequency))
	stopifnot(!any(duplicated(tt)))
	time(x) <- tt
	x <- merge(x, zoo(, seq(as.integer(round(start * frequency)), 
		tail(tt,1))))
	ts(coredata(x), start = start, frequency = frequency)
}

