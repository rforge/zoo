\name{value}
\alias{value}
\alias{value.default}
\alias{value<-}
\alias{value<-.default}
\title{Extract/Replace the Value of Objects}
\description{
Generic functions for extracting the value of a (more complex)
object and replacing it.
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
The default methods extract the value of the object (i.e., stripping off
all attributes) or replacing to it.
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
