# runmean, runmax, runmed (, runmad) based on code posted by Jarek Tuszynski at
# https://www.stat.math.ethz.ch/pipermail/r-help/2004-October/057363.html

#Z# currently these work only for zoo objects, but maybe these should really
#Z# be generics with something like the run*0() functions as the default.

runmean <- function(x, k, na.pad = TRUE) { 
	stopifnot(k <= NROW(x))
	index.x <- index(x)
	if (!na.pad) index.x <- index.x[-seq(k-1)]
	runmean0 <- function(x, k, na.rm) {
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
		return(zoo(runmean0(x, k, na.rm), index.x))
	else
		return(zoo(apply(x, 2, runmean0, k=k, na.rm=na.rm), index.x))
}

runmax <- function(x, k, na.pad = TRUE) { 
	stopifnot(k <= NROW(x))
	index.x <- index(x)
	if (!na.pad) index.x <- index.x[-seq(k-1)]
	runmax0 <- function(x, k, na.pad) {
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
		return(zoo(runmax0(x, k, na.pad), index.x))
	else
		return(zoo(apply(x, 2, runmax0, k=k, na.pad=na.pad), index.x))
}

runmed <- function(x, k, na.pad = TRUE, ...) { 
	stopifnot(all(!is.na(x)), k <= NROW(x), k %% 2 == 1)
	# todo:
	# rather than abort we should do a simple loop to get the medians
	# for those columns with NAs.
	index.x <- index(x)
	m <- k %/% 2
	n <- NROW(x)
	if (!na.pad) index.x <- index.x[k:n]
	runmed0 <- function(x, k, na.pad, ...) {
		x <- stats::runmed(x, k, ...)[-c(seq(m),seq(to=n,len=m))]
		if (na.pad) x <- c(rep(NA,k-1), x)
		x
	}
	if (length(dim(x)) == 0)
		return(zoo(runmed0(x, k, na.pad = na.pad, ...), index.x))
	else
		return(zoo(apply(x, 2, runmed0, k=k, na.pad=na.pad, ...), index.x))
}

# todo:
# runmad <- function()
