as.Date.numeric <- function(x, ...)
	structure(floor(x + .001), class = "Date")

as.Date.integer <- function(x, ...)
	structure(x, class = "Date")

as.Date.ts <- function(x, ...) {
   # if by is "days" then times of x are treated as days since as.Date(0);
   # otherwise, they are treated as last two digits of years if 1st time < 100
   # or years if 1st time > 100.  If 1st time < yearcutoff then century is 
   # assumed to be 2000; otherwise, it is assumed to be 1900. Default for
   # yearcutoff is 25.
   args <- list(...)
   yearcutoff <- if (is.null(args$yearcutoff)) 25 else args$yearcutoff
   by <- if (is.null(args$by)) 1 else pmatch(args$by, c("years", "days"))
   time.x <- unclass(time(x))
   if (by == 2 ) {
	stopifnot(frequency(x) == 1)
	return(as.Date(time.x))
   }
   if (time.x[1] <= yearcutoff) 
	time.x <- time.x + 2000
   else if (time.x[1] < 100)
	time.x <- time.x + 1900
   if (frequency(x) == 1)
	as.Date(paste(time.x, 1, 1, sep = "-"))
   else if (frequency(x) == 4)
	as.Date(paste((time.x + .001) %/% 1, 3*(cycle(x)-1)+1, 1, sep = "-"))
   else if (frequency(x) == 12)
	as.Date(paste((time.x + .001) %/% 1, cycle(x), 1, sep = "-"))
   else
	stop("unable to convert ts time to Date class")
}

