\name{zoo}
\alias{zoo.intersect}
\alias{zoo.intersect}
\alias{zoo.union}
\title{Bind Two or More zoo Objects}
\description{
Bind \code{"zoo"} objects. \code{zoo.union} pads
with \code{NA}s to the total index coverage, \code{zoo.intersect} restricts to
the those indexes contained in all objects. 
}

\usage{
zoo.intersect(\dots)
zoo.union(\dots)
}

\arguments{
  \item{\dots}{two or more objects of class \code{"zoo"}.}
}

\details{
Both functions should behave similar to the corresponding 
functions for \code{"ts"} objects: \code{\link{ts.union}} and
\code{\link{ts.intersect}}.
The indexes of different
\code{"zoo"} objects can be of different classes and are coerced to
one class in the resulting object.
}

\value{
An object of class \code{"zoo"}.
}

\seealso{\code{\link{zoo}}}

\examples{
x.date <- as.POSIXct(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.date)

y1 <- zoo(matrix(1:10, ncol = 2), 1:5)
y2 <- zoo(matrix(rnorm(10), ncol = 2), 3:7)

zoo.intersect(y1, y2)
zoo.union(x, y1, y2)
}
\keyword{ts}
