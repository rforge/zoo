\name{defaultfrequency}
\alias{defaultfrequency}
\title{ Default frequency }
\description{
  Frequency associated with a series.
}
\usage{
defaultfrequency(x)
}
\arguments{
  \item{x}{ times }
}
\details{
  This is a generic function.  Methods are available for 
  class \code{Date} and \code{yearmon}.
}
\value{
  The \code{Date} method returns 1.  The \code{yearmon} method
  returns 12.  If no method has been defined for the time
  class then by default the frequency
  is the reciprocal of the least difference between two time
  points.  If time is not coercable to \code{numeric} then 
  the result is an error.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
defaultfrequency(as.Date(1:10))
defaultfrequency(as.yearmon(1950:2000))
defaultfrequency(.1:1)
}
