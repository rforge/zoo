\name{dynlm}
\alias{dynlm}
\alias{dynlm.model.frame}
\alias{model.frame.zoo}
\alias{model.frame.AsIs}
\concept{dynamic regression}
\title{Fitting Dynamic Linear Regression Models}
\description{
  \code{dynlm} is used to fit linear models.
  It can be used to carry out regression,
  single stratum analysis of variance and
  analysis of covariance (although \code{\link{aov}} may provide a more
  convenient interface for these).
}
\usage{
dynlm(formula, data, subset, weights, na.action,
   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
   singular.ok = TRUE, contrasts = NULL, offset = NULL,
   preserve.data = TRUE, \dots)
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.
    The details of model specification are given below.}

  \item{data}{an optional data frame containing the variables
    in the model. \emph{This argument is currently unsupported.}
    Currently, the variables are taken from \code{environment(formula)},
    typically the environment from which \code{dynlm} is called.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{weights}{an optional vector of weights to be used
    in the fitting process. If specified, weighted least squares is used
    with weights \code{weights} (that is, minimizing \code{sum(w*e^2)});
    otherwise ordinary least squares is used.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Other possible values include
    \code{\link{na.contiguous}} and \code{\link{na.locf}}. }

  \item{method}{the method to be used; for fitting, currently only
    \code{method = "qr"} is supported; \code{method = "model.frame"} returns
    the model frame (the same as with \code{model = TRUE}, see below).}

  \item{model, x, y, qr}{logicals.  If \code{TRUE} the corresponding
    components of the fit (the model frame, the model matrix, the
    response, the QR decomposition) are returned.}

  \item{singular.ok}{logical. If \code{FALSE} (the default in S but
    not in \R) a singular fit is an error.}

  \item{contrasts}{an optional list. See the \code{contrasts.arg}
    of \code{model.matrix.default}.}

  \item{offset}{this can be used to specify an \emph{a priori}
    known component to be included in the linear predictor
    during fitting.  An \code{\link{offset}} term can be included in the
    formula instead or as well, and if both are specified their sum is used.}

  \item{preserve.data}{logical. If \code{TRUE} the original data is 
    stored in an additional slot \code{original.data} of the returned
    object.}

  \item{\dots}{additional arguments to be passed to the low level
    regression fitting functions (see below).}
}
\details{
  The function \code{dynlm} modifies \code{\link[stats]{lm}} such that
  dynamic regressions based on (numeric) zoo objects can be specified.
  Note, that this code is under development and its behaviour and interface
  might change in future versions.

  Models for \code{dynlm} are specified symbolically using formulae as in
  \code{link[stats]{lm}}.   All variables must be zoo objects.  
}

\value{
  An object of class \code{"dynlm"} which inherits from \code{"lm"}
  is returned.
}

\examples{
z1 <- zoo(1:10)
f <- factor(gl(2,5))

# use lm with zoo object and factor object
lm(z1 ~ f)

# convert factor object to numeric and finally zoo
# so dynlm can be used
dynlm(z1 ~ zoo(as.numeric(f)-1))

# note that last example used this model frame
dynlm.model.frame(z1 ~ zoo(as.numeric(f)-1))

# and this model matrix
model.matrix(z1 ~ zoo(as.numeric(f)-1))

z2 <- z1*z1
z3 <- z2*z2

dynlm(lag(z1) ~ z2 + diff(z3))

z1[4] <- NA
dynlm.model.frame(z1 ~ z2, na.action = na.locf)
dynlm(z1 ~ z2, na.action = na.locf)

}
\keyword{regression}
