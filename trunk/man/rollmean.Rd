\name{rollmean}
\alias{rollmean}
\alias{rollmax}
\alias{rollmed}
\alias{rollmean.zoo}
\alias{rollmed.zoo}
\alias{rollmax.zoo}
\alias{rollmean.default}
\alias{rollmed.default}
\alias{rollmax.default}
\title{Rolling Means/Maximums/Medians}
\description{
Generic functions for computing rolling means, maximums and medians of \code{"zoo"}
objects.
}
\usage{
rollmean(x, k, na.pad = FALSE)
rollmax(x, k, na.pad = FALSE)
rollmed(x, k, na.pad = FALSE, \dots)
}
\arguments{
  \item{x}{a \code{"zoo"} object.}
  \item{k}{integer width of the rolling window. Must be odd
    for \code{rollmed}.}  
  \item{na.pad}{logical. Add \code{NA} padding at beginning.}
  \item{\dots}{Further arguments passed. Only applicable to \code{rollmed}.}
}

\details{
These functions are similar to \code{\link{rapply}} but are
optimized for speed.  The extra arguments available are
those accepted by \code{mean}, \code{max} and \code{med}.
}

\value{
A \code{"zoo"} object with the rolling mean/max/median.
}

\seealso{\code{\link{zoo}}}

\examples{
x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
x <- zoo(rnorm(12), x.Date)

rollmean(x, 3)
rollmax(x, 3)
rollmed(x, 3)

xm <- zoo(matrix(1:12, 4, 3), x.Date[1:4])
rollmean(xm, 3)
rollmax(xm, 3)
rollmed(xm, 3)

rapply(xm, 3, mean) # uses rollmean
rapply(xm, 3, function(x)mean(x)) # does not use rollmean

}
\keyword{ts}

