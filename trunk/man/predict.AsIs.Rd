\name{predict.AsIs}
\alias{predict.AsIs}
\title{Model Predictions}
\usage{
\method{predict}{AsIs}(object, newdata, \dots)
}
\arguments{
  \item{object}{a model object for which prediction is desired.}
  \item{newdata}{list containing zoo objects to be used for prediction.}
  \item{\dots}{additional arguments affecting the predictions produced.}
}
\description{
  \code{predict.AsIs} calls \code{predict} and then coerces the
  result to the appropriate class, usually \code{zoo} or \code{ts}.
}
\seealso{
  \code{\link{predict}}
}
\examples{
x.lm <- lm(I(gdp ~ lag(ipi, -1)), 
               list(gdp = zoo(1:10), ipi = zoo(1:10)^2))
predict(I(x.lm), list(ipi = zoo(11:15)))
}
\keyword{methods}
