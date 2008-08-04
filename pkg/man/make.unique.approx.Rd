\name{make.unique.approx}
\alias{make.unique.approx}
\alias{make.unique.approx.default}
\alias{make.unique.incr}
\alias{make.unique.incr.Date}
\alias{make.unique.incr.default}
\alias{make.unique.incr.yearmon}
\alias{make.unique.incr.yearqtr}
\title{ Make a vector unique }
\description{
  Make a vector unique 
}
\usage{
make.unique.approx(x, ...)
make.unique.incr(x, incr, ...)
}
\arguments{
  \item{x}{Vector.  This is a vector of a class which has differences which can be coerced to numeric such that the difference when added to the first value returns the
second value. }
  \item{incr}{numeric. Increment successive values by this amount.}
  \item{\dots}{ Other arguments.  Currently the only one is \code{quantile}
which is specified as a number between 0 and 1 such that if it is 0 then
the first time value among duplicates is kept as is and the others calculated
or if 1 then the last time value is kept as is or if an intermediate fraction
is used then that quantile of the position is kept as is.}
}

\value{
For \code{make.unique.approx}
the function returns \code{x} except among any set of repeated elements
the one at the position whose quantile is indicated is left as is and
all others are linearly interpolated.

\code{make.unique.incr} has \code{Date}, \code{yearmon}, \code{yearqtr}
and \code{default} methods.  It is similar to
\code{make.unique.approx} except there is an \code{incr} argument and
the other values rather than being linearly interpolated are incremented
by the value of the \code{incr} argument.  The default value of the
\code{incr} argument is 1 for \code{Date}, 1/12 for \code{yearmon},
1/4 for \code{yearqtr} and a value chosen sufficiently small for the
\code{default}.   In the case any run generates points
outside of the interval defined by its neighbours an error is generated.
}
\seealso{ \code{\link{read.zoo}} }
\examples{
xx <- c(10, rep(20, 10), 30)
make.unique.approx(xx)

Sys.Date() + xx
make.unique.incr(Sys.Date() + xx)
make.unique.incr(Sys.Date() + xx, quantile = 0)
make.unique.incr(Sys.Date() + xx, quantile = .5)
}
\keyword{ ts }
