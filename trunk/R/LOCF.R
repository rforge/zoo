LOCF <- function(x, ...)
	UseMethod("LOCF")

LOCF.default <- function(x, ...) {
      x <- unclass(x)
      L <- !is.na(x)
      x[c(NA,which(L))[cumsum(L)+1]]
   }
	
LOCF.zoo <- function(x, ...) {
   x[] <- if (length(dim(x)) == 0)
      LOCF.default(x)
   else
      apply(x, 2, LOCF.default)
   x
}
