\name{zoo}
\alias{zoo}
\alias{print.zoo}
\alias{summary.zoo}
\alias{str.zoo}
\alias{is.zoo}
\alias{[.zoo}
\alias{head.zoo}
\alias{tail.zoo}
\alias{Ops.zoo}
\alias{t.zoo}
\alias{cumsum.zoo}
\alias{cumprod.zoo}
\alias{cummin.zoo}
\alias{cummax.zoo}
\alias{window.zoo}
\title{Z's Ordered Observations}
\description{
\code{zoo} is the creator for an S3 class of indexed
totally ordered observations which includes irregular
time series.
}
\usage{
zoo(x = NA, order.by = index(x))
\method{print}{zoo}(x, style = <<see below>>, quote = FALSE, \dots) 
}
\arguments{
  \item{x}{a vector or matrix.}
  \item{order.by}{a vector by which the observations in \code{x}
    are ordered.}
  \item{style}{a string specifying the printing style which can be
    \code{"horizontal"} (the default for vectors), \code{"vertical"}
    (the default for matrices) or \code{"plain"} (which first prints
    first the data and then the index).}
  \item{quote}{logical. Should characters be quoted?}
  \item{\dots}{further arguments passed to the print methods of
    the data and the index.}
}

\details{
\code{zoo} provides infrastructure for ordered observations
which are stored internally in a vector or matrix with an
index attribute (with in principle arbitrary class, see below)
which has to be of the same length as \code{NROW(x)}.

\code{zoo} is particularly aimed at irregular time series of numeric
vectors/matrices. \code{zoo}'s key design goals are independence of a particular
index/date/time class and consistency (to the extent possible)
with ts and base R by providing methods to standard generics. Therefore,
standard functions can be used to work with \code{"zoo"} objects and no
new set of commands has to be employed.

The vector of indexes \code{order.by} can be of arbitrary class,
but it is essential that \code{order(order.by)} works. For other
functions it is assumed that \code{c()}, \code{length()},
\code{match()} and subsetting \code{[,} work.

If \code{x} is shorter or longer than \code{order.by} it is
expanded accordingly.


Methods to standard generics for \code{"zoo"} objects currently
include: \code{print}, \code{summary}, \code{str}, \code{head}, \code{tail},
\code{[} (subsetting), \code{rbind}, \code{cbind}, \code{merge}
(see \code{\link{merge.zoo}})

Working on data

Working on index attribute



is still under development, but it provides \code{plot}
and \code{index} (or \code{time}) methods for \code{"zoo"} objects
and can convert \code{"ts"} and \code{"irts"} objects with
\code{\link{as.zoo}}.


Working on data and index

\code{window}, 


In addition to standard group generic function (see \code{\link{Ops}}),
the following mathematical operations are availabe as methods for
\code{"zoo"} objects: transpose \code{t} which coerces to a matrix 
first, and \code{cumsum}, \code{cumprod}, \code{cummin}, \code{cummax}
which are applied column wise.

Coercion to and from \code{"zoo"} objects, see \code{\link{as.zoo}}.

}

\value{
A vector or matrix with an \code{"index"} attribute of the same
dimension (\code{NROW(x)}) by which \code{x} is ordered.
}

\seealso{\code{\link{plot.zoo}}, \code{\link{index}}, \code{\link{merge.zoo}}}

\examples{
## simple creation and plotting
x.date <- as.POSIXct(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.date)
plot(x)
time(x)

## subsetting
x[1:3]

# create a constant series
z <- zoo(1, seq(4)[-2])

# create a series with zero columns
z0 <- zoo(, 1:4)[,-1]
}
\keyword{ts}
