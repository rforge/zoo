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

#Z# I think restricting cbind (as opposed to merge) might be a good
#Z# idea, along the lines of:
 
cbind.zoo <- function(..., all = TRUE, fill = NA, suffixes = NULL)
{
  merge.zoo(..., all = all, fill = fill, suffixes = suffixes, retclass = "zoo")
}

#Z# instead of:
#Z# cbind.zoo <- 
merge.zoo <- function(..., all = TRUE, fill = NA, suffixes = NULL, retclass = c("zoo", "list"))
{
    if (!is.null(retclass)) retclass <- match.arg(retclass)
    # arg is a possibly named list with the ... args as char strings
    cl <- as.list(match.call())
    cl[[1]] <- cl$all <- cl$fill <- cl$retclass <- cl$suffixes <- NULL
    args <- lapply(cl, deparse)

    parent <- parent.frame()

    # ensure all ... args are of class zoo
    stopifnot(all(sapply(args, function(x) is.zoo(eval(parse(text=x), parent)))))
    # use argument names if suffixes not specified
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
        # suffixes <- makeNames(as.list(substitute(list(...)))[-1])
        suffixes <- makeNames(cl)
    }
    if (length(suffixes) != length(args)) {
        warning("length of suffixes and does not match number of merged objects")
        suffixes <- rep(suffixes, length.out = length(args))
    }

    # extend all to a length equal to the number of args
    all <- rep(all, length.out = length(args))

    # ensure the class of the index of each arg are all the same
    indexlist <- lapply(args, function(x) index(eval(parse(text=x), parent)))
    indexclasses <- sapply(indexlist, function(x) class(x)[1])
    if (!all(indexclasses == indexclasses[1])) 
        warning(paste("Index vectors are of different classes:", 
            paste(indexclasses, collapse = " ")))

    # fn to get the unique elements in x, in sorted order, using only
    # [, match, length and order
    sort.unique <- function(x) {
        x <- x[match(x, x) == seq(length = length(x))]
        x[order(x)]
    }

    # fn to get intersection of each element in list of lists
    intersect.list <- function(list) { 
        my.table <- function(x) {
           x <- x[order(x)]
           table(match(x, x))
	}
	union <- do.call("c", list)
	sort.unique(union)[ my.table(union) == length(list) ]
    }
    indexintersect <- intersect.list(indexlist)

    # get the indexes of the final answer which is the union of
    # all indexes of args corresponding to all=TRUE with the intersection
    # of all indexes
    indexunion <- do.call("c", indexlist[all])
    if (is.null(indexunion)) indexunion <- do.call("c", indexlist)[0]
    indexes <- sort.unique(c(indexunion, indexintersect))

    # the f function does the real work
    # it takes a zoo object, a, and fills in a matrix corresponding to
    # indexes with the values in a.  If ret.zoo is TRUE if it is to return
    # a zoo object.  If ret.zoo is FALSE it simply returns with the matrix
    # just calculated.  Otherwise, converts that matrix into a zoo object.
    # match is convenience wrapper for match with nomatch=0 default
    match0 <- function(a, b, nomatch = 0, ...) match(a, b, nomatch = nomatch, 
        ...)
    f <- function(a, ret.zoo = TRUE) {
        if (length(a) == 0) 
            return(matrix(nr = length(indexes), nc = 0))
        z <- matrix(fill, length(indexes), NCOL(a))
        z[match0(index(a), indexes), ] <- if (length(dim(a)) == 0)
		a[match0(indexes, index(a))]                      
	else                                                      
		a[match0(indexes, index(a)),,drop = FALSE]        
 	if (!ret.zoo) return(z)
	if (length(dim(a)) == 0) {
		zoo(z[,1,drop=TRUE], indexes)
	} else {
		colnames(z) <- colnames(a)
		zoo(z, indexes)
	}
    }
    # if all contains only FALSE elements then the following f is used
    # instead of the prior f for performance purposes.  If all contains
    # only FALSE then the resulting index is the intersection of the
    # index of each argument so we can just return a[index] or a[index,].
    # Also if we are not to return a zoo object then unclass it prior to return.
    if (!any(all)) f <- function(a, ret.zoo = TRUE) {
	if (!ret.zoo) class(a) <- NULL
	a <- if (length(dim(a)) == 0)
		a[match0(indexes, attr(a,"index"))]
	else
		a[match0(indexes, attr(a,"index")),,drop=FALSE]
    }
    # if retclass is NULL do not provide a return value but instead
    # update each argument that is a variable, i.e. not an expression,
    # in place.  
    if (is.null(retclass)) {
        for(vn in args) 
           tryCatch(
	     eval(substitute(v <- f(v), list(f = f, v = as.name(vn))), parent), 
	     condition = function(x)NULL
           )
	invisible(return(NULL))
    } 

    # apply f to each arg put result of performing this on all args in list rval
    # and then cbind that list together to produce the required matrix
    rval <- lapply(args, function(x) 
		f(eval(parse(text=x), parent), ret.zoo = retclass == "list"))
    names(rval) <- args
    if (retclass == "list") return(rval)
    rval <- do.call("cbind", rval)

    #
    # processing from here on is to compute nice column names
    #

    if (length(unlist(sapply(eval(parse(text=args), parent), colnames))) > 0) {
        fixcolnames <- function(i) {
            a <- eval(parse(text=args[[i]]), parent)
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
    } else {
        fixcolnames <- function(a) {
            if (length(a) == 0) 
                return(NULL)
            if (NCOL(a) < 2) 
                return("")
            else return(paste(".", 1:NCOL(a), sep = ""))
        }
        zoocolnames <- lapply(args, function(i) 
			fixcolnames(eval(parse(text=i), parent)))
        zoocolnames <- unlist(lapply(seq(along = args), function(i) if (is.null(zoocolnames[[i]])) 
            NULL
        else paste(suffixes[i], zoocolnames[[i]], sep = "")))
        colnames(rval) <- make.unique(zoocolnames)
    }
    zoo(rval, indexes)
}

