rapply <- function(data, width, FUN, by = 1, ascending = TRUE, by.column = TRUE, na.pad = FALSE, ...)
    UseMethod("rapply")

rapply.zoo <- function(data, width, FUN, by = 1, ascending = TRUE, by.column = TRUE, na.pad = FALSE, ...) {
        itt <- 0
	embedi <- function(n,k,by=1,ascending=FALSE) {
	# n = no of time points, k = number of columns
	# by = increment. normally =1 but if =b calc every b-th point 
	# ascending If TRUE, points passed in ascending order else descending.
	# Note that embed(1:n,k) corresponds to embedi(n,k,by=1,rev=TRUE)
	# e.g. embedi(10,3)
		s <- seq(1,n-k+1,by)
		lens <- length(s)
		cols <- if (ascending) 1:k else k:1
		matrix(s + rep(cols,rep(lens,k))-1,lens)
	}

    if (by.column && by == 1 && ascending && is.null(list(...))) 
	switch(deparse(substitute(FUN)),
		mean = return(rollmean(data, width, na.pad = na.pad)),
		max = return(rollmax(data, width, na.pad = na.pad)),
		median = return(rollmed(data, width, na.pad = na.pad)))
    nr <- NROW(data)
    width <- as.integer(width)[1]
    stopifnot( width > 0, width <= nr )
    tt <- index(data)[seq(width, nr, by)]
    res <- if (is.null(dim(data)))
	   zoo(apply( embedi(nr, width, by, ascending), 1, 
                function(st) FUN(data[st], ...)), tt, 
			if (by==1) frequency(data))
    else if (by.column) {
	    e <- embedi(nr, width, by, ascending)
	    zoo( sapply( 1:ncol(data), function(i)
			apply( e, 1, function(st) FUN(data[st,i], ...) ) ),
			tt, if (by == 1) frequency(data)
	    )
    } else
	   zoo(apply( embedi(nr, width, by, ascending), 1, 
                function(st) FUN(data[st,], ...)), tt, 
			if (by ==1) frequency(data))
    if (na.pad) merge(res, zoo(,index(data), frequency(data))) else res
} 

