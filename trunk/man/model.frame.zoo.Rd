\name{model.frame.zoo}
\alias{model.frame.AsIs}
\alias{model.frame.zoo}
\alias{model.frame.ts}
\concept{dynamic regression}
\title{Model Frame for zoo Series}
\description{
  \code{model.frame.zoo} is used to create model frames for zoo series.
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
  When fitting models to data based on a formula specification, the data
  handling is usually done by the default method to the generic function
  \code{model.frame}. For example \code{lm(formula)} dispatches to
  \code{model.frame.default}. In some situations, it might be desirable
  to dispatch to another function that does the data handling depending
  on the class of the variables that are specified in \code{formula}.
  This is not (easily) possible in R, neither using S3 nor S4 methods,
  because the first argument \code{formula} is of class \code{"formula"}
  and it is not straightforwardly possible to dispatch on the data specified
  by it. To work around this, \code{zoo} provides a function \code{model.frame.AsIs}
  which is called if the model is specified by \code{lm(I(formula))}, i.e.,
  the \code{formula} is insulated by \code{I()} providing an object of
  class \code{"AsIs"} (and not \code{"formula"}). The function \code{model.frame.AsIs}
  examines the data specified by the formula and re-dispatches to a
  \code{model.frame} method depending on the class of the dependent variable.  
  Thus, if the dependent variable specified in \code{I(formula)} is of
  class \code{"foo"} the method \code{model.frame.foo} will be called for
  handling the data.

  If the dependent variable in such a model is of class \code{"zoo"} and
  \code{model.frame.zoo} will be called. Its  key role is to inspect a formula
  that contains only \code{zoo} objects as variables and transform
  it to a model frame that can be used in various regression functions
  appropriately aligning the various series. Similarly, a \code{model.frame.ts}
  method is provided for \code{"ts"} objects. Note, that despite their names
  these function do \emph{not} expect a \code{"zoo"} or \code{"ts"} object
  as their respective first argument, they expect a \code{formula}.
  Their behaviour is essentially the same as in the default \code{\link{model.frame}}
  method, but they retain the index/time information. Furthermore, it
  enables the user to use \code{\link{diff}} and \code{\link{lag}} in the
  model specification.
  
  As many regression functions in R use the same steps to extract the 
  data from a specified \code{formula}, this approach modularizes the
  data management and regression based on \code{"zoo"} objects is
  easily available using various regression functions. Hence, the user will
  usually not have to call any of the \code{model.frame} functions explicitely
  but only has to insulate the \code{formula}. See the examples for an illustration.
  (The regression functions for which this approach is known to work includes \code{\link[stats]{lm}},
  \code{\link[stats]{glm}}, \code{\link[Design]{lrm}}, \code{\link[MASS]{lqs}},
  \code{\link[nnet]{nnet}}, \code{\link[e1071]{svm}}, \code{\link[quantreg]{rq}},
  \code{\link[randomForest]{randomForest}} and possibly many others.)  
}

\value{
  A \code{\link{data.frame}} containing the variables used in
  \code{formula} plus those specified \code{\dots}.
}
\seealso{\code{\link{model.frame}}}


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
if(require(MASS))
lqs(I(lag(yz) ~ diff(xz)+fz))

# neural network
if(require(nnet))
nnet(I(lag(yz) ~ diff(xz)+fz), size = 1)

# quantile regression
if(require(quantreg))
rq(I(lag(yz) ~ fz), tau = 0.25)

# random forest
set.seed(1)
if(require(randomForest))
randomForest(I(lag(yz) ~ diff(xz) + fz))

# support vector machine
if(require(e1071))
svm(I(lag(yz) ~ diff(xz) + fz))

}
\keyword{regression}

