# rollmean, rollmax, rollmed (, runmad) based on code posted by Jarek Tuszynski at
# https://www.stat.math.ethz.ch/pipermail/r-help/2004-October/057363.html
# ToDo: runmad, currently rapply() can be used

rollmean <- function(x, k, na.pad = FALSE)
  UseMethod("rollmean")

rollmean.default <- function(x, k, na.pad = FALSE)
{
  x <- unclass(x)
  n <- length(x) 
  y <- x[k:n] - x[c(1, 1:(n-k))] # difference from previous
  y[1] <- sum(x[1:k])		 # find the first
  # apply precomputed differencest sum
  rval <- cumsum(y)/k
  if (na.pad) rval <- c(rep(NA, k-1), rval)
  return(rval)
}

rollmean.zoo <- function(x, k, na.pad = FALSE) { 
  stopifnot(k <= NROW(x))
  index.x <- index(x)
  if(!na.pad) index.x <- index.x[-seq(k-1)]
  if(length(dim(x)) == 0) 
    return(zoo(rollmean.default(x, k, na.pad), index.x))
  else
    return(zoo(apply(x, 2, rollmean.default, k=k, na.pad=na.pad), index.x))
}


rollmax <- function(x, k, na.pad = FALSE)
  UseMethod("rollmax")

rollmax.default <- function(x, k, na.pad = FALSE)
{
  n <- length(x) 
  rval <- rep(0, n) 
  a <- 0
  for (i in k:n) {
  rval[i] <- if (is.na(a) || is.na(rval[i=1]) || a==rval[i-1]) 
      max(x[(i-k+1):i]) # calculate max of window
  else 
      max(rval[i-1], x[i]); # max of window = rval[i-1] 
  a <- x[i-k+1] # point that will be removed from window
  }
  if (na.pad) rval[seq(k-1)] <- NA
    else rval <- rval[-seq(k-1)]
  return(rval)
} 

rollmax.zoo <- function(x, k, na.pad = FALSE, ...) { 
  stopifnot(k <= NROW(x))
  index.x <- index(x)
  if (!na.pad) index.x <- index.x[-seq(k-1)]
  if (length(dim(x)) == 0) 
    return(zoo(rollmax.default(x, k, na.pad), index.x))
  else
    return(zoo(apply(x, 2, rollmax.default, k=k, na.pad = na.pad), index.x))
}


rollmed <- function(x, k, na.pad = FALSE, ...)
  UseMethod("rollmed")

rollmed.default <- function(x, k, na.pad = FALSE, ...)
  stats::runmed(x, k, ...)

rollmed.zoo <- function(x, k, na.pad = FALSE, ...) { 
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
		return(zoo(apply(x, 2, rollmed0, k = k, na.pad = na.pad, ...), index.x))
}


