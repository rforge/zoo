na.omit.zoo <- function (object, ...) 
{
    if (!is.atomic(object)) 
        return(object)
    d <- dim(object)
    if (length(d) > 2) 
        return(object)
    omit <- seq(along = object)[is.na(object)]
    if (length(omit) == 0) 
        return(object)
    if (length(d)) {
        omit <- unique(((omit - 1)%%d[1]) + 1)
        nm <- rownames(object)
        object <- object[-omit, , drop = FALSE]
    }
    else {
        nm <- names(object)
        object <- object[-omit]
    }
    if (any(omit)) {
        names(omit) <- nm[omit]
    }
    object
}


na.contiguous <- function(frame)
	UseMethod("na.contiguous")
	
na.contiguous.default <- function(frame)
	stats::na.contiguous(x)
	
na.contiguous.zoo <- function(frame) 
{
    tm <- index(frame)
    xfreq <- frequency(frame)
    if (length(dim(frame)) > 0) 
        good <- apply(!is.na(frame), 1, all)
    else good <- !is.na(frame)
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
    n <- NROW(frame)
    if (st > 1) 
        omit <- c(omit, 1:(st - 1))
    if (en < n) 
        omit <- c(omit, (en + 1):n)
    if (length(omit)) {
        frame <- if (length(dim(frame)) == 2) 
            frame[st:en, ]
        else frame[st:en]
    }
    frame
}
