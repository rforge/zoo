
na.approx <- function(y, x, FUN, na.rm = TRUE, ...) UseMethod("na.approx")

# interpolates y using x which defaults to seq(along = y) if FUN
# is missing or to FUN(time(y)) if not
na.approx.default <- function(y, 
	x = if (missing(FUN)) seq(len=length(time(y))) else FUN(time(y)), 
	FUN, na.rm = TRUE, ...)
{
	# na.approx is based on post from r-help by 
	# Sundar Dorai-Raj <sundar.dorai-raj@PDF.COM>
	na.approx.0 <- function(y) {
		na <- is.na(y)
		if(all(!na)) return(y)
		y[na] <- approx(x[!na], y[!na], x[na], ...)$y
		y
	}

        y[] <- if (length(dim(y)) == 0)
                na.approx.0(y)
        else
                apply(y, 2, na.approx.0)
        if (na.rm) na.omit(y) else y
}

