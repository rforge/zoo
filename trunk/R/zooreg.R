zooreg <- function(x, start = 1, end = numeric(), frequency = 1, 
  deltat = 1, ts.eps = getOption("ts.eps"), order.by = NULL)
{
    ## if no index (i.e., order.by) is specified: behave as ts()
    ## else: behave as zoo()

    if (is.null(order.by)) {
        if (missing(x) || is.null(x)) x <- NA
	if(!any(c(is.vector(x), is.factor(x), is.matrix(x), is.data.frame(x))))
  	    stop(paste(dQuote("x"), ": attempt to define illegal zoo object"))
	ndata <- NROW(x)        

        ## choose frequency/deltat
        if (missing(frequency)) frequency <- 1/deltat
            else if (missing(deltat)) deltat <- 1/frequency
        if (frequency > 1 && abs(frequency - round(frequency)) < ts.eps)
            frequency <- round(frequency)
        ## choose start/end
        if (length(start) > 1) start <- start[1] + (start[2] - 1)/frequency
        if (length(end) > 1) end <- end[1] + (end[2] - 1)/frequency
        if (missing(end)) end <- start + (ndata - 1)/frequency
            else if (missing(start)) start <- end - (ndata - 1)/frequency
        if (start > end) stop("start cannot be after end")

        ## check whether lengths of data and index match
	order.by <- seq(start, end, by = deltat)
	nobs <- length(order.by)
        ## nobs <- floor((end - start) * frequency + 1.01)
        if (nobs != ndata) {
	  if(is.vector(x)) x <- rep(x, length.out = nobs)
	  else if(is.factor(x)) x <- factor(rep(as.character(x), length.out = nobs), labels = levels(x))
	  else if(is.matrix(x) || is.data.frame(x)) x <- x[rep(1:ndata, length.out = nobs), , drop = FALSE]
        }

        attr(x, "oclass") <- attr(x, "class")
        attr(x, "index") <- order.by
        attr(x, "frequency") <- frequency
        class(x) <- c("zooreg", "zoo")
        return(x)
    } else {
        return(zoo(x, order.by, frequency))
    }
}
