\name{window.zoo}
\alias{window.zoo}
\alias{window<-}
\alias{window<-.zoo}
\title{Extract/Assign to Time Windows of Objects}
\description{
Generic functions for extracting time windows
of (zoo) objects and assigning a value to it.
}

\usage{
\method{window}{zoo}(x, index = index.zoo(x), start = NULL, end = NULL, \dots)

window(x, \dots) <- value

\method{window}{zoo}(x, index = index.zoo(x), start = NULL, end = NULL, \dots) <- value
}

\arguments{
  \item{x}{an object.}
  \item{index}{the index/time window which should be extracted.}
  \item{start}{an index/time value. Only the indexes in \code{index}
    which are greater or equal to \code{start} are used.}
  \item{end}{an index/time value. Only the indexes in \code{index}
    which are lower or equal to \code{end} are used.}
  \item{value}{a suitable value object for use with \code{window(x)}.}
  \item{\dots}{currently not used.}
}

\value{
The methods for \code{"zoo"} objects either extract the time window of the
object (and hence return a \code{"zoo"} object) or assign a value to it.
}

\seealso{\code{\link{zoo}}}

\examples{
x.date <- as.POSIXct(paste("2003-", rep(1:4, 4:1), "-", sample(1:28, 10, replace = TRUE), sep = ""))
x <- zoo(matrix(rnorm(20), ncol = 2), x.date)

window(x, start = as.POSIXct("2003-02-01"), end = as.POSIXct("2003-03-01"))
window(x, index = x.date[1:6], start = as.POSIXct("2003-02-01"))
window(x, index = x.date[c(4, 8, 10)])
window(x, index = x.date[c(4, 8, 10)]) <- matrix(1:6, ncol = 2)
x
}
\keyword{ts}
