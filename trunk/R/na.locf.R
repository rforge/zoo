na.locf <- function(x, na.rm = TRUE, ...)
	UseMethod("na.locf")

na.locf.default <- function(x, na.rm = TRUE, ...) {
	na.locf.0 <- function(x) {
	      L <- !is.na(x)
	      x[c(NA,which(L))[cumsum(L)+1]]
	}
	x[] <- if (length(dim(x)) == 0)
		na.locf.0(x)
	else
		apply(x, 2, na.locf.0)
	if (na.rm) na.omit(x) else x
}

na.contiguous <- function(x)
	UseMethod("na.contiguous")
	
na.contiguous.ts <- function(x)
	stats::na.contiguous(x)

na.contiguous.default <- function (x) 
{
    if (length(dim(x)) == 2) 
        good <- apply(!is.na(x), 1, all)
    else good <- !is.na(x)
    if (!sum(good)) 
        stop("all times contain an NA")
    tt <- cumsum(!good)
    ln <- sapply(0:max(tt), function(i) sum(tt == i))
    seg <- (seq(along = ln)[ln == max(ln)])[1] - 1
    keep <- (tt == seg)
    st <- min(which(keep))
    if (!good[st]) 
        st <- st + 1
    en <- max(which(keep))
    omit <- integer(0)
    n <- NROW(x)
    if (st > 1) 
        omit <- c(omit, 1:(st - 1))
    if (en < n) 
        omit <- c(omit, (en + 1):n)
    cl <- class(x)
    if (length(omit)) {
        x <- if (length(dim(x))) 
            x[st:en, ]
        else x[st:en]
        attr(omit, "class") <- "omit"
        attr(x, "na.action") <- omit
        if (!is.null(cl)) 
            class(x) <- cl
    }
    x
}

