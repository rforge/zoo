\name{model.frame.zoo}
\alias{model.frame.AsIs}
\alias{model.frame.zoo}
\alias{model.frame.ts}
\concept{dynamic regression}
\title{Model Frame for zoo Series}
\description{
  \code{model.frame.zoo} is used to create model frames for zoo series.
  It can be used in conjunction with lm, glm, 
Design::lrm, 
e1071::svm, 
MASS::lqs 
quantreg::rq, 
randomForest::randomForest, 
and possibly other regression-like routines.  It is normally called
from lm, glm and similar routines although it can be called
directly by the user.
}
\usage{

\method{model.frame}{AsIs}(formula, data = NULL, subset = NULL, 
    na.action = na.omit, drop.unused.levels = FALSE, xlev = NULL, \dots)
\method{model.frame}{zoo}(formula, data = NULL, subset = NULL, 
    na.action = na.omit, drop.unused.levels = FALSE, xlev = NULL, \dots)
\method{model.frame}{ts}(formula, data = NULL, subset = NULL, 
    na.action = na.omit, drop.unused.levels = FALSE, xlev = NULL, \dots)

}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.
    The details of model specification are given below.}

  \item{data}{in the case of \code{model.frame.zoo} this is an
    optional list of \code{objects} or a single \code{zoo} object.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Other possible values include
    \code{\link{na.approx}},
    \code{\link{na.contiguous}} and \code{\link{na.locf}}. }


  \item{drop.unused.levels}{should factors have unused levels dropped?
    Defaults to \code{FALSE}.}

  \item{xlev}{a named list of character vectors giving the full set of levels
    to be assumed for each factor.}

  \item{\dots}{additional arguments to be passed to the low level
    regression fitting functions (see below).}
}
\details{

  \code{model.frame.AsIs} is normally called from various regression
  functions such as \code{lm} and its use is indicated by the user
  by surrounding the 
  formula with I() in such regression functions as shown in the examples 
  below.  It examines the 
  dependent, i.e. the
  reponse, variable in the formula and uses its class to redispatch 
  \code{model.frame}.  Currently \code{model.frame.zoo} and
  \code{model.frame.ts} are provided to be used in this way.

  The key role of \code{model.frame.zoo} is to inspect a formula
  that contains only \code{zoo} objects as variables and transform
  it to a model frame that can be used in various regression functions
  appropriately aligning the various series.  It is normally called
  by \code{model.frame.AsIs}.  \code{model.frame.ts} is similar but
  for \code{ts} objects.

  If either \code{formula} or \code{data} is already a model frame (a
  data frame with a \code{"terms"} attribute and the other is missing,
  the model frame is returned.  Unless \code{formula} is a terms object,
  \code{terms} is called on it.  (If you wish to use the
  \code{keep.order} argument of \code{terms.formula}, pass a terms
  object rather than a formula.)

  All the variables in \code{formula}, \code{subset} and in \code{\dots}
  are looked for first in \code{data} and then in the environment of
  \code{formula} (see the help for \code{\link{formula}()} for further
  details) and collected into a data frame. Then the \code{subset}
  expression is evaluated, and it is is used as a row index to the data
  frame. Then the \code{na.action} function is applied to the data frame
  (and may well add attributes).  The levels of any factors in the data
  frame are adjusted according to the \code{drop.unused.levels} and
  \code{xlev} arguments.

  Note that model.frame.AsIs examines the class of the response variable
  and dispatches model.frame.X if its class is X.  Thus specifying
  a formula enclosed in I(\dots) with a zoo response variable as the
  formula argument of a regression model will have the effect of allowing 
  zoo objects to be used as in the examples below.
}
\value{
  A \code{\link{data.frame}} containing the variables used in
  \code{formula} plus those specified \code{\dots}.
}
\seealso{\code{\link{model.matrix}} for the \dQuote{design matrix},
  \code{\link{formula}} for formulas  and
  \code{\link{expand.model.frame}} for model.frame manipulation.
}


\examples{


	yz <- zoo(1:12)^2
	xz <- zoo(1:9)^2
	fz <- zoo(gl(2,5))

	# example of dispatching on response, using diff and
	# lag, using a factor and returning residuals and fitted 
	# zoo objects
	yz.lm <- lm(I(lag(yz) ~ diff(xz) + fz))
	residuals(yz.lm)
	fitted(yz.lm)

	# examples of using list in data argument
	lm(I(lag(y) ~ diff(x) + f), list(y = yz, x = xz, f = fz))
	lm(I(y ~ x+f), list(y=lag(yz), x=diff(xz), f=fz))

	# examples of using a zoo object in the data argument
	lm(I(lag(y) ~ diff(x)), merge(y=yz, x=xz))
	lm(I(lag(yz) ~ diff(xz)), merge(yz, xz))
	lm(I(y ~ x), merge(y = lag(yz), x = diff(xz)))

	# using a variety of other modelling functions
	# note that residuals and fitted may or may not
	# work with these and if they do work may not
	# return zoo objects.

	# resistant regression
	library(MASS)
	lqs(I(lag(yz) ~ diff(xz)+fz))

	# rank regression
	if (require(quantreg))
		rq(I(lag(yz) ~ fz), tau = 0.25)

	# random forest
	set.seed(1)
	if (require(randomForest))
		randomForest(I(lag(yz) ~ diff(xz) + fz))

	# support vector machine
	if (require(e1071))
		svm(I(lag(yz) ~ diff(xz) + fz))

}
\keyword{regression}

