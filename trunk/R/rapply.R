rapply <- function(data, width, FUN, MARGIN = 1, by.column = TRUE, na.pad = TRUE, ...)
    UseMethod("rapply")

rapply.zoo <- function(data, width, FUN, MARGIN = 1, by.column = TRUE, na.pad = TRUE, ...) {
    if (by.column) switch(deparse(substitute(FUN)),
		mean = return(rollmean(data, width, na.pad = na.pad)),
		max = return(rollmax(data, width, na.pad = na.pad)),
		median = return(rollmed(data, width, na.pad = na.pad)))
    nr <- NROW(data)
    width <- as.integer(width)[1]
    stopifnot( width > 0, width <= nr )
    res <- if (!is.null(dim(data)) && by.column)
	    apply( embed(1:nr, width), 1, 
                function(st) apply(data[st,], 2, FUN, ...))
    else
	   apply( embed(1:nr, width), 1, 
                function(st) FUN(data[st,], ...))
    if (na.pad) { if (is.null(dim(res)))
	zoo(c(rep(NA, width-1), res), index(data))
    else
	zoo(rbind(matrix(NA, nr=width-1, nc=ncol(res)), res), index(data))
    } else
	zoo(res, index(data)[-seq(len = width - 1)])
} 

