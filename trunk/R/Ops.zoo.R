Ops.zoo <- function (e1, e2) 
{
    if (missing(e2)) {
        NextMethod(.Generic)
    }
    else if (any(nchar(.Method) == 0)) {
        NextMethod(.Generic)
    }
    else {
	vec <- function(x) {
		dim(x) <- NULL
		x
	}
        nc1 <- NCOL(e1)
	cn1 <- colnames(e1)
        nc2 <- NCOL(e2)
	cn2 <- colnames(e2)
	e12 <- merge(e1, e2, all = FALSE)
        e1 <- if (length(dim(e1)) > 0) 
            e12[, 1:nc1, drop = FALSE]
        else vec(e12[, 1])
	colnames(e1) <- cn1
        e2 <- if (length(dim(e2)) > 0)
            e12[, nc1 + (1:nc2), drop = FALSE]
        else vec(e12[, nc1 + 1])
	colnames(e2) <- cn2
        NextMethod(.Generic)
    }
}

t.zoo <- function(x)
	t(as.matrix.zoo(x))
 
cumsum.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cumsum(x)
	  else x[] <- apply(x, 2, cumsum)
	return(x)
}


cumprod.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cumprod(x)
	  else x[] <- apply(x, 2, cumprod)
	return(x)
}


cummin.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cummin(x)
	  else x[] <- apply(x, 2, cummin)
	return(x)
}


cummax.zoo <- function(x) 
{
	if (length(dim(x)) == 0) x[] <- cummax(x)
	  else x[] <- apply(x, 2, cummax)
	return(x)
}
