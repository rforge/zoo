\name{as.Date.numeric}
\alias{as.Date.numeric}
\alias{as.Date.integer}
\alias{as.Date.ts}

\title{Date Conversion Functions from Numeric, Integer and ts Class}
\description{
  Functions to convert numeric and related class to objects of
  class \code{"Date"} representing calendar dates.
}
\usage{
\method{as.Date}{numeric}(x, \dots)

\method{as.Date}{integer}(x, \dots)
\method{as.Date}{ts}(x, offset = 0, \dots)

}
\arguments{
  \item{x}{An object to be converted.  See details.}
  \item{offset}{A value added to time(x).}
  \item{\dots}{Further arguments.  Currently not used.}
}
\details{
  In the case of \code{as.Date.integer}
    the argument is interpreted as the number of days since the origin where
    January 1, 1970 corresponds to 0.  Negative values are allowed.

    In the case of a numeric argument the floor of the argument is taken
    and then interpreted as with integer.
    i.e. since January 1, 1970.

  In the case of \code{as.Date.ts} the time scale is regarded as 
  years or days according to the value of \code{by} if frequency(x) is 1. 
  If frequency(x) is 4 then the time scale is regarded to be years and 
  quarters and if frequency(x) is 12 then the time scale is regarded to
  be years and months.
}
\value{
  The \code{as.Date} methods return an object of class \code{"\link{Date}"}.
  In the case of \code{as.Date.ts} applied to a yearly, quarterly or monthly
  series the earliest date, i.e. first of year, quarter or month, is returned.
}
\seealso{
  \link{Date} for details of the date class;
}
\examples{

as.Date(0) # origin, i.e. 1970-01-01

# all three result in origin and next 9 days
as.Date(0:9)
as.Date(0) + 0:9
myts <- ts(rnorm(10), start = 0)
as.Date(unclass(time(myts)))

# both result in ten firsts of year starting 2001-01-01
as.Date(ts(rnorm(10)), offset = 2000)
as.Date(ts(rnorm(10), start = 2001))  

# both result in ten firsts of month starting with 1990-01-01
as.Date(ts(rnorm(10), start = c(90,1), freq = 12), offset = 1900)  
as.Date(ts(rnorm(10), start = c(1990,1), freq = 12))  

}
\keyword{utilities}
\keyword{chron}
