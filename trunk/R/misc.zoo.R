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

as.zoo.ts <- function(x, 
   timeclass = if (frequency(x) == 12) "yearmon" else "numeric", ...)
{
  tt <- unclass(time(x))
  tt <- eval(parse(text=paste("as", timeclass, sep = ".")))(tt)
  zoo(coredata(x), tt)
} 

as.yearmon <- function(x) UseMethod("as.yearmon")
as.yearmon.numeric <- function(x) structure(x, class = "yearmon")
as.yearmon.integer <- function(x) structure(x, class = "yearmon")
as.yearmon.default <- function(x) 
  structure( with(unclass(as.POSIXlt(x,tz="GMT")), 1900+year+mon/12), 
	class = "yearmon")
as.Date.yearmon <- function(x) {
	x <- unclass(x)
	year <- floor(x + .001)
	month <- floor(12 * (x - year) + 1 + .5 + .001)
	as.Date(paste(year, month, 1, sep = "-"))
}
format.yearmon <- function (x, format = "%b %Y", ...) 
{
    xx <- format(as.POSIXlt(as.Date(x)), format = format, ...)
    names(xx) <- names(x)
    xx
}
as.character.yearmon <- function(x) format(x)
print.yearmon <- function(x) { print(format(x)); invisible(x) }
"[.yearmon" <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}
axis.yearmon <- function (side, x, at, format, ...) 
	axis.Date(side, as.Date(x), at, format, ...)


###########################################################
# as.ts.zoo
###########################################################
# project time onto regularly spaced grid
as.ts.zoo <- function(y) {
	fit <- function(y, x=seq(0,length=length(y))) { 
		b <- cov(x,y) / var(x)
		a <- mean(y) - b * mean(x)
		c(a, b)
	}
	coef <- fit(as.numeric(time(y)))
	ts(y, start = coef[1], deltat = coef[2])
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
spacedness <- function(x) cor(as.numeric(time(x)), seq(0, length(x)))


###########################################################
# model.frame generic -- replaces same function in stats
###########################################################
# dispatch on class of response variable or failing that on class of formula
model.frame <- 
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
model.frame.zooformula <- model.frame.zoo
model.frame.tsformula <- model.frame.ts

# example
# x <- y <- zoo(1:10)^4
# lm(lag(y) ~ diff(x,2))  # No I() !!!!!
