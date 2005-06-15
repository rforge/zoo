\name{as.ts.zoo}
\title{Convert zoo to ts}
\description{
  The method \code{as.ts.zoo} is used to convert \code{zoo} objects
  to \code{ts} class.
}
\usage{

\method{as.ts}{zoo}(x, start = as.numeric(time(x[1])),
   frequency = defaultfrequency(time(x), ts.eps = ts.eps), deltat = 1,
   ts.eps = getOption("ts.eps"), ...)

}
\arguments{
  \item{x}{A zoo object.}
  \item{start}{the time of the first observation.}
  \item{frequency}{the number of observations per unit of time.}
  \item{deltat}{the fraction of the sampling period between successive
    observations; e.g., 1/12 for monthly data.  Either
    \code{frequency} or \code{deltat} or neither should be provided.}
  \item{ts.eps}{time series comparison tolerance.  Frequencies are
    considered equal if their absolute difference is less than
    \code{ts.eps}.}
  \item{\dots}{additional arguments.  Currently not used.}
}
\details{
  Create a \code{ts} object from a \code{zoo} object.  The time class
  of the \code{zoo} object must support \code{as.numeric}.  The resulting
  \code{ts} object will have the indicated \code{frequency}.  Alternately,
  \code{deltat} which is \code{1/frequency} may be specified.  If neither
  \code{frequency} nor \code{deltat} is specified then the frequency
  is obtained from \code{defaultfrequency(x)}.  
}
\seealso{
  \code{\link{defaultfrequency}},
  \code{\link{start}},
  \code{\link{time}},
  \code{\link{window}};
}
\examples{
# monthly ts series
x <- ts(1:20, freq = 12, start = c(1950,2))
z <- as.zoo(x)
# remove one element
z <- z[-3]
# convert back.  Removed element will be an NA.
as.ts(z)
}
\keyword{ts}
