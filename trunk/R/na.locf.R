na.locf <- function(object, na.rm = TRUE, ...)
	UseMethod("na.locf")

na.locf.default <- function(object, na.rm = TRUE, ...) {
	na.locf.0 <- function(object) {
	      L <- !is.na(object)
	      object[c(NA,which(L))[cumsum(L)+1]]
	}
	object[] <- if (length(dim(object)) == 0)
		na.locf.0(object)
	else
		apply(object, 2, na.locf.0)
	if (na.rm) na.omit(object) else object
}

na.contiguous <- function(object, ...)
	UseMethod("na.contiguous")

## The former (up to R 2.0.0) na.contiguous should become
## na.contiguous.ts:
na.contiguous.ts <- function(object, ...)
	stats::na.contiguous(object, ...)

na.contiguous.default <- function(object, ...) 
{
    if (length(dim(object)) == 2) 
        good <- apply(!is.na(object), 1, all)
    else good <- !is.na(object)
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
    n <- NROW(object)
    if (st > 1) 
        omit <- c(omit, 1:(st - 1))
    if (en < n) 
        omit <- c(omit, (en + 1):n)
    cl <- class(object)
    if (length(omit)) {
        object <- if (length(dim(object))) 
            object[st:en, ]
        else object[st:en]
        attr(omit, "class") <- "omit"
        attr(object, "na.action") <- omit
        if (!is.null(cl)) 
            class(object) <- cl
    }
    object
}

