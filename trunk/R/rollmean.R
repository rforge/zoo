# rollmean, rollmax, rollmed (, runmad) based on code posted by Jarek Tuszynski at
# https://www.stat.math.ethz.ch/pipermail/r-help/2004-October/057363.html

#Z# currently these work only for zoo objects, but maybe these should really
#Z# be generics with something like the run*0() functions as the default.

rollmean <- function(x, k, na.pad = TRUE) UseMethod("rollmean")
rollmean.zoo <- function(x, k, na.pad = TRUE) { 
	stopifnot(k <= NROW(x))
	index.x <- index(x)
	if (!na.pad) index.x <- index.x[-seq(k-1)]
	rollmean0 <- function(x, k, na.rm) {
		x    <- unclass(x)
		n    <- length(x) 
		y    <- x[ k:n ] - x[ c(1,1:(n-k)) ] # difference from previous
		y[1] <- sum(x[1:k])                  # find the firs 
		# apply precomputed differencest sum
		if (na.pad)
			c(rep(NA, k-1), cumsum(y)/k)
		else
			cumsum(y)/k
	}
	if (length(dim(x)) == 0) 
		return(zoo(rollmean0(x, k, na.rm), index.x))
	else
		return(zoo(apply(x, 2, rollmean0, k=k, na.rm=na.rm), index.x))
}

rollmax <- function(x, k, na.pad = TRUE) UseMethod("rollmax")
rollmax.zoo <- function(x, k, na.pad = TRUE) { 
	stopifnot(k <= NROW(x))
	index.x <- index(x)
	if (!na.pad) index.x <- index.x[-seq(k-1)]
	rollmax0 <- function(x, k, na.pad) {
           n <- length(x) 
           y <- rep(0, n) 
           a <- 0
           for (i in k:n) {
		   y[i] <- if (is.na(a) || is.na(y[i=1]) || a==y[i-1]) 
			max(x[(i-k+1):i]) # calculate max of window
		   else 
			max(y[i-1], x[i]); # max of window = y[i-1] 
		   a <- x[i-k+1] # point that will be removed from window
           }
 	   if (na.pad) y[seq(k-1)] <- NA else y <- y[-seq(k-1)]
	   y
        } 
	if (length(dim(x)) == 0) 
		return(zoo(rollmax0(x, k, na.pad), index.x))
	else
		return(zoo(apply(x, 2, rollmax0, k=k, na.pad=na.pad), index.x))
}

rollmed <- function(x, k, na.pad = TRUE, ...) UseMethod("rollmed")
rollmed.default <- function(x, k, na.pad = TRUE, ...) stats::runmed(x, k, ...)
rollmed.zoo <- function(x, k, na.pad = TRUE, ...) { 
	stopifnot(all(!is.na(x)), k <= NROW(x), k %% 2 == 1)
	# todo:
	# rather than abort we should do a simple loop to get the medians
	# for those columns with NAs.
	index.x <- index(x)
	m <- k %/% 2
	n <- NROW(x)
	if (!na.pad) index.x <- index.x[k:n]
	rollmed0 <- function(x, k, na.pad, ...) {
		x <- stats::runmed(x, k, ...)[-c(seq(m),seq(to=n,len=m))]
		if (na.pad) x <- c(rep(NA,k-1), x)
		x
	}
	if (length(dim(x)) == 0)
		return(zoo(rollmed0(x, k, na.pad = na.pad, ...), index.x))
	else
		return(zoo(apply(x, 2, rollmed0, k=k, na.pad=na.pad, ...), index.x))
}

# todo:
# runmad <- function()

