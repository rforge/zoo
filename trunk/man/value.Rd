\name{value}
\alias{value}
\alias{value.zoo}
\alias{value<-}
\alias{value<-.zoo}
\title{Extract/Assign to the Value of Objects}
\description{
Generic functions for extracting the value of a (more complex)
object and assigning a value to it.
}

\usage{
value(x, \dots)
value(x) <- value
}

\arguments{
  \item{x}{an object.}
  \item{value}{a suitable value object for use with \code{x}.}
  \item{\dots}{currently not used.}
}

\value{
The methods for \code{"zoo"} objects either extract the value of the
object, i.e., the observations without the index/time attribute, or
assign a value to it (and hence return an object of class \code{"zoo"}).
}

\seealso{\code{\link{zoo}}}

\examples{
x.date <- as.POSIXct(paste("2003-", rep(1:4, 4:1), "-", sample(1:28, 10, replace = TRUE), sep = ""))
x <- zoo(matrix(rnorm(20), ncol = 2), x.date)

## the full time series
x
## and only matrix of observations
value(x)

## change the observations
value(x) <- matrix(1:20, ncol = 2)
x
}
\keyword{ts}
