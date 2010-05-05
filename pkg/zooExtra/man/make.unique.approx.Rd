\name{make.unique.approx}
\alias{make.unique.approx}
\alias{make.unique.approx.default}
\title{ Make a vector unique }
\description{
  Make a vector unique 
}
\usage{
make.unique.approx(x, ...)
\method{make.unique.approx}{default}(x, quantile = 0:1, ...)
}
\arguments{
  \item{x}{Vector.  Vector of a class which has differences that can be coerced to numeric for which the difference, when added to the first value, returns the
second value. }
  \item{quantile}{A number between 0 and 1 (or vector of numbers) 
such that if it is 0 then
the first time value among duplicates is kept as is and the others
calculated via linear interpolation or if 1 then the last time value is
kept as is and the others calculated via linear interpolation.  If
an intermediate fraction is used then that quantile is kept as is.
If a vector of numbers is specified then they are applied in succession.
0 will not remove duplicates at the end and 1 will not remove trailing
duplicates at the beginning but \code{0:1} or \code{1:0} will remove both.}
   \item{\dots}{Further arguments.}
}

\value{
The function returns \code{x} except among any set of repeated elements
the one at the position whose quantile is indicated is left as is and
all others are linearly interpolated.

}
\examples{
xx <- c(10, rep(20, 10), 30)
make.unique.approx(xx)

}
\keyword{ ts }
