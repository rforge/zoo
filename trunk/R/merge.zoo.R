rbind.zoo <- function(..., deparse.level = 1)
{  
  args <- list(...)
  indexes <- do.call("c", lapply(args, index))

  my.table <- function(x) {
    x <- x[order(x)]
    table(match(x,x))
  }
  if(max(my.table(indexes)) > 1) stop("indexes overlap")

  ncols <- sapply(args, NCOL)  
  if(!all(ncols == ncols[1])) stop("number of columns differ")

  if(ncols[1] > 1)
    zoo(do.call("rbind", lapply(args, unclass)), indexes)
  else
    zoo(do.call("c", lapply(args, unclass)), indexes)
}

cbind.zoo <-
merge.zoo <- function (..., all = TRUE, fill = NA, suffixes = NULL) 
{
    args <- list(...)
    if (!all(unlist(lapply(args, is.zoo)))) 
        stop("all arguments must be zoo objects")
    if (is.null(suffixes)) {
        makeNames <- function(l) {
            nm <- names(l)
            fixup <- if (is.null(nm)) 
                seq(along = l)
            else nm == ""
            dep <- sapply(l[fixup], function(x) deparse(x)[1])
            if (is.null(nm)) 
                return(dep)
            if (any(fixup)) 
                nm[fixup] <- dep
            nm
        }
        suffixes <- makeNames(as.list(substitute(list(...)))[-1])
    }
    if (length(suffixes) != length(args)) {
        warning("length of suffixes and does not match number of merged objects")
        suffixes <- rep(suffixes, length.out = length(args))
    }
    all <- rep(all, length(args))
    indexlist <- lapply(args, index)
    indexclasses <- sapply(indexlist, function(x) class(x)[1])
    if (!all(indexclasses == indexclasses[1])) 
        warning(paste("Index vectors are of different classes:", 
            paste(indexclasses, collapse = " ")))

    sort.unique <- function(x) {
        x <- x[match(x, x) == seq(length = length(x))]
        x[order(x)]
    }
    intersect.list <- function(list) { 
        my.table <- function(x) {
           x <- x[order(x)]
           table(match(x, x))
	}
	union <- do.call("c", list)
	sort.unique(union)[ my.table(union) == length(list) ]
    }
    indexintersect <- intersect.list(indexlist)

    indexunion <- do.call("c", indexlist[all])
    indexes <- do.call("c", indexlist)
    if (is.null(indexunion)) 
        indexunion <- indexes[0]
    indexes <- sort.unique(c(indexunion, indexintersect))
    match0 <- function(a, b, nomatch = 0, ...) match(a, b, nomatch = nomatch, 
        ...)
    f <- function(a) {
        if (length(a) == 0) 
            return(matrix(nr = length(indexes), nc = 0))
        z <- matrix(fill, length(indexes), NCOL(a))
        z[match0(index(a), indexes), ] <- a[match0(indexes, index(a)), 
            , drop = FALSE]
        return(z)
    }
    rval <- do.call("cbind", lapply(args, f))
    if (length(unlist(sapply(args, colnames))) > 0) {
        fixcolnames <- function(i) {
            a <- args[[i]]
            if (length(a) == 0) 
                return(NULL)
            if (NCOL(a) < 2) {
                return("")
            }
            else {
                rval <- colnames(a)
                if (is.null(rval)) {
                  rval <- paste(1:NCOL(a), suffixes[i], sep = ".")
                }
                else {
                  rval[rval == ""] <- as.character(which(rval == 
                    ""))
                }
                return(rval)
            }
        }
        zoocolnames <- lapply(seq(along = args), fixcolnames)
        zcn <- unlist(zoocolnames)
        fixme <- lapply(zoocolnames, function(x) x %in% zcn[duplicated(zcn)])
        f <- function(i) {
            rval <- zoocolnames[[i]]
            rval[rval == ""] <- suffixes[i]
            rval
        }
        zoocolnames <- lapply(seq(along = args), f)
        f <- function(i) ifelse(fixme[[i]], paste(zoocolnames[[i]], 
            suffixes[i], sep = "."), zoocolnames[[i]])
        if (any(duplicated(unlist(zoocolnames)))) 
            zoocolnames <- lapply(seq(along = args), f)
        colnames(rval) <- make.unique(unlist(zoocolnames))
    }
    else {
        fixcolnames <- function(a) {
            if (length(a) == 0) 
                return(NULL)
            if (NCOL(a) < 2) 
                return("")
            else return(paste(".", 1:NCOL(a), sep = ""))
        }
        zoocolnames <- lapply(args, fixcolnames)
        zoocolnames <- unlist(lapply(seq(along = args), function(i) if (is.null(zoocolnames[[i]])) 
            NULL
        else paste(suffixes[i], zoocolnames[[i]], sep = "")))
        colnames(rval) <- make.unique(zoocolnames)
    }
    z <- zoo(rval, indexes)
    z
}

