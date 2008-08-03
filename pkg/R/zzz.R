

.onLoad <- function(libname, pkgname) {
	unlockBinding("as.Date.numeric", baseenv())
	assign("as.Date.numeric", function (x, origin, ...) {
		if (missing(origin)) origin <- "1970-01-01"
		as.Date(origin, ...) + x} , baseenv())
	lockBinding("as.Date.numeric", baseenv())
}

.onUnload <- function(libpath) {
	unlockBinding("as.Date.numeric", baseenv())
	assign("as.Date.numeric", function (x, origin, ...) {
		if (missing(origin)) 
			stop("'origin' must be supplied")
		as.Date(origin, ...) + x}, baseenv())
	lockBinding("as.Date.numeric", baseenv())
}


