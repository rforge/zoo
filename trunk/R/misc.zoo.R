# This file is intended to be sourced on top of zoo 0.9-1
# Assuming zoo 0.9-1 is installed load this file like this:
# source("misc.zoo.r")
# (There is a require statement near the top that will load zoo.)
#
# It defines new classes:
# 1. zooformula, tsformula for formula objects that 
#    have a zoo and ts response, respectively.
# 2. yearmon which is a date class consistent with ts 
#    useful for dates that only have a year and month.
#    as.zoo.ts is changed to use it in the frequency=12 case.
# 3. zooregspaced which is a subclass for regularly spaced zoo
#    series.  Not much done on this one.
#
# It also provides a new model.frame generic (overwriting 
# the one that comes with R) that uses a modified dispatch 
# scheme in which dispatch is done on the class of:
# 1. the formula response variable if a model.frame method 
#    for that class is available or
# 2. the formula itself otherwise
# This allows elimination of the I() that was previously
# required.
#
# Note to myself:
# Changes need to be done to model.frame.zoo.Rd, as.zoo.Rd
# and a new yearmon.Rd needs to be developed.  Also vignette.
# We should change the randomForest example in the vignette
# to rq from quantreg since Andy mentioned to me that 
# he has never used randomForest with time series.
# We could also add a yearquarter class.
# 

# source() this file 
require(zoo)

###########################################################
# zooformula
###########################################################

as.zooformula <- function(x) UseMethod("as.zooformula")
as.zooformula.formula <- function(x) 
	structure(x, class = c("zooformula", class(x)))

###########################################################
# tsformula
###########################################################
as.tsfomula <- function(x) UseMethod("as.tsformula")
as.tsformula.formula <- function(x) 
	structure(x, class = c("tsformula", class(x)))

###########################################################
# yearmon
#
# class for dates which have only year and month.  
# Same internal representation that ts uses for freq=12.
# NB. as.zoo.ts uses yearmon if frequency of ts series is 12.
# NB. An alternate internal representation might have been months since Epoch.
###########################################################

#Z# this was moved to other files and probably can be eliminated
#Z# 
#Z# as.zoo.ts <- function(x, 
#Z#    timeclass = if (frequency(x) == 12) "yearmon" else "numeric", ...)
#Z# {
#Z#   tt <- unclass(time(x))
#Z#   tt <- eval(parse(text=paste("as", timeclass, sep = ".")))(tt)
#Z#   zoo(coredata(x), tt)
#Z# } 

#Z# as.yearmon <- function(x) UseMethod("as.yearmon")
#Z# as.yearmon.numeric <- function(x, unit = "year") switch(unit,
#Z# 	year = structure(x, class = "yearmon"),
#Z# 	month = structure(x/12, class = "yearmon"))
#Z# as.yearmon.integer <- function(x, unit = "year") switch(unit,
#Z# 	year = structure(x, class = "yearmon"),
#Z# 	month = structure(x/12, class = "yearmon"))
#Z# as.yearmon.default <- function(x) 
#Z#   structure( with(as.POSIXlt(x,tz="GMT"), 1900+year+mon/12), 
#Z# 	class = "yearmon")
#Z# as.Date.yearmon <- function(x, ...) {
#Z# 	x <- unclass(x)
#Z# 	year <- floor(x + .001)
#Z# 	month <- floor(12 * (x - year) + 1 + .5 + .001)
#Z# 	as.Date(paste(year, month, 1, sep = "-"))
#Z# }
#Z# format.yearmon <- function (x, format = "%b %Y", ...) 
#Z# {
#Z#     xx <- format(as.Date(x), format = format, ...)
#Z#     names(xx) <- names(x)
#Z#     xx
#Z# }
#Z# as.character.yearmon <- function(x) format(x)
#Z# print.yearmon <- function(x, ...) { 
#Z# 	print(format(x), ...)
#Z# 	invisible(x) 
#Z# }
#Z# "[.yearmon" <- function (x, ..., drop = TRUE) 
#Z# {
#Z#     cl <- oldClass(x)
#Z#     class(x) <- NULL
#Z#     val <- NextMethod("[")
#Z#     class(val) <- cl
#Z#     val
#Z# }
#Z# axis.yearmon <- function (side, x, at, format, ...) 
#Z# 	axis.Date(side, as.Date(x), at, format, ...)


###########################################################
# as.ts.zoo
###########################################################
# old - project time onto regularly spaced grid
#as.ts.zoo <- function(y) {
#	fit <- function(y, x=seq(0,length=length(y))) { 
#		b <- cov(x,y) / var(x)
#		a <- mean(y) - b * mean(x)
#		c(a, b)
#	}
#	coef <- fit(as.numeric(time(y)))
#	ts(y, start = coef[1], deltat = coef[2])
#}


# if frequency is 0 then project time onto regularly spaced
# grid.  If frequency is missing then use 0 if the series is
# regular, 12 if the series has index class "yearmon", 1 if
# the series has index class "Date" and 0 otherwise.
old.as.ts.zoo <- function(y, frequency) {
   tt <- time(y)
   if (missing(frequency)) { frequency <-
	if (identical(all.equal(spacedness(y),1), TRUE))
		0
	else 
		defaultfrequency(time(y))
   }
   if (frequency) {
	tt <- time(y) <- as.numeric(tt)
	tt <- seq(floor((frequency * tt[1]) + .0001)/frequency, 
		tail(tt,1), 1/frequency)
	merge(y, zoo(, tt), all = c(FALSE, TRUE), retclass = NULL)
	ts(coredata(y), start = tt[1], freq = frequency)
   } else {
	fit <- function(y, x=seq(0,length=length(y))) { 
		b <- cov(x,y) / var(x)
		a <- mean(y) - b * mean(x)
		c(a, b)
	}
	coef <- fit(as.numeric(tt))
	ts(coredata(y), start = coef[1], deltat = coef[2])
   }
}

old.as.ts.zoo <- function(x, start = as.numeric(time(x[1])), 
		frequency = defaultfrequency(time(x))) {
	tt <- round(as.numeric(time(x)) * frequency)
	stopifnot(!any(duplicated(tt)))
	time(x) <- tt
	x <- merge(x, zoo(, seq(round(start * frequency), tail(tt,1))))
	ts(coredata(x), start = start, frequency = frequency)
}


###########################################################
# zooregspaced
###########################################################
as.zooregspaced <- function(x) UseMethod("as.zooregspaced")
# as.zooreg.zoo <- function(x) ... tbd ...
# as.zooregspaced.ts <- function(x) ... tbd ...
# as.zoo.zooregspaced <- function(x) ...
# spacedness ranges from 
#  0 for randomly spaced points to
#  1 for regularly spaced points
spacedness <- function(x) {
	tt <- time(x)
	cor(as.numeric(tt), seq(0, length = length(tt)))
}

###########################################################
# model.frame generic -- replaces same function in stats
###########################################################
# dispatch on class of response variable or failing that on class of formula
### change this from .model.frame to model.frame to turn it on
.model.frame <- 
function (formula, data = NULL, subset = NULL, na.action = na.omit, 
	drop.unused.levels = FALSE, xlev = NULL, ...) 
{
	if (is.null(data)) data <- parent.frame()
	if (inherits(formula, "formula")) {
		env <- environment(formula)
		response <- eval(as.list(formula)[[2]], as.list(data), env)
		newclass <- paste(class(response), "formula", sep = "")
		class(formula) <- c(newclass, class(formula))
	}
	UseMethod("model.frame", formula)
}


# since this file is intended to be sourced on top of zoo we just
# assign model.frame.zoo and model.frame.ts to the new names but they
# should actually be replaced in the final code

#Z# these are not present in zoo anymore
#Z# model.frame.zooformula <- model.frame.zoo
#Z# model.frame.tsformula <- model.frame.ts

# example
# x <- y <- zoo(1:10)^4
# lm(lag(y) ~ diff(x,2))  # No I() !!!!!


########

defaultfrequency <- function(x, ...) UseMethod("defaultfrequency")
defaultfrequency.Date <- function(x, ...) 1
defaultfrequency.yearmon <- function(x, ...) 12
defaultfrequency.default <- function(x, ts.eps = getOption("ts.eps"), ...) {
	d <- diff(as.numeric(x))
	deltat <- min(d)
	frequency <- 1/deltat
	if (frequency > 1 && abs(frequency - round(frequency)) < ts.eps) 
		frequency <- round(frequency)
	stopifnot(max(abs(frequency*d - round(frequency*d))) < ts.eps)
	frequency
}

as.ts.zoo <- function(x, start = as.numeric(time(x[1])),
   frequency = defaultfrequency(time(x), ts.eps = ts.eps), deltat = 1,
   ts.eps = getOption("ts.eps"), ...) {
	if (missing(deltat)) deltat <- 1/frequency else frequency <- 1/deltat
	round. <- function(x) deltat * round(x/deltat)
	tt <- round.(as.numeric(time(x)))
	tt2 <- round.(seq(head(tt,1), tail(tt,1), deltat))
	xx <- merge(zoo(coredata(x), tt), zoo(, tt2))
	ts(coredata(xx), start = start, frequency = frequency)
}


