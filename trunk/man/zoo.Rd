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
\alias{na.contiguous}
\alias{na.contiguous.default}
\alias{na.contiguous.zoo}
\alias{order}
\alias{order.default}
\alias{match}
\alias{match.default}
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

When creating a \code{"zoo"} object with the function \code{zoo},
the vector of indexes \code{order.by} can be of (a single) arbitrary class
(if \code{x} is shorter or longer than \code{order.by} it is
expanded accordingly),
but it is essential that \code{order(order.by)} works. For other
functions it is assumed that \code{c()}, \code{length()},
\code{match()} and subsetting \code{[,} work. If this is not the case
for a particular index/date/time class, then methods for these 
generic functions should be created. Note, that to achieve this
the non-generic base functions \code{\link{order}} and \code{\link{match}}
are made generics in \code{zoo} with the base functions being the
default (see \code{\link[base]{order}} and \code{\link[base]{match}}).

Methods to standard generics for \code{"zoo"} objects currently
include: \code{print} (see above), \code{summary}, \code{str}, \code{head},
\code{tail}, \code{[} (subsetting), \code{rbind}, \code{cbind}, \code{merge}
(see \code{\link{merge.zoo}}), \code{aggregate} (see \code{\link{aggregate.zoo}}),
\code{plot} and \code{lines} (see \code{\link{plot.zoo}}).

Additionally, \code{zoo} provides several generic functions and methods
to work (a) on the value or data contained in a \code{"zoo"} object, (b) the
index (or time) attribute associated to it, and (c) on both data and
index:

(a) The data contained in \code{"zoo"} objects can be extracted by
\code{value} (strips off all \code{"zoo"}-specific attributes) and modified
using \code{value<-}. Both are new generic functions with methods for
\code{"zoo"} objects, see \code{\link{value}}.

(b) The index associated with a \code{"zoo"} object can be extracted
by \code{index} and modified by \code{index<-}. As the interpretation
of the index as ``time'' in time series applications is more natural,
there are also synonymous methods \code{time} and \code{time<-}. The
start and the end of the index/time vector can be queried by
\code{start} and \code{end}. See \code{\link{index}}.

(c) To work on both data and index/time, \code{zoo} provides methods
\code{lag}, \code{diff} (see \code{\link{lag.zoo}}) and \code{window}, 
\code{window<-} (see \code{\link{window.zoo}}).

In addition to standard group generic function (see \code{\link{Ops}}),
the following mathematical operations are availabe as methods for
\code{"zoo"} objects: transpose \code{t} which coerces to a matrix 
first, and \code{cumsum}, \code{cumprod}, \code{cummin}, \code{cummax}
which are applied column wise.

Coercion to and from \code{"zoo"} objects is available for objects of
various classes, in particular \code{"ts"}, \code{"irts"} and \code{"its"}
objects can be coerced to \code{"zoo"}, the reverse is available for
\code{"its"} and for \code{"irts"} (the latter in package \code{tseries}).
Furthermore, \code{"zoo"} objects can be coerced to vectors, matrices and
data frames (dropping the index/time attribute). See \code{\link{as.zoo}}.

Three methods are available for \code{NA} handling in the data of 
\code{"zoo"} objects: \code{\link{na.omit}} which returns a \code{"zoo"}
object with incomplete observations removed, \code{\link{na.locf}} which
replaces \code{NA}s by the last previous non-\code{NA} and \code{\link{na.contiguous}}
which extracts the longest consecutive stretch of non-missing values
in a \code{"zoo"} object. Note, that the latter function is made a
generic in \code{zoo} with the stats function being the method for
\code{"ts"} objects.
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

## different classes of indexes/times can be used, e.g. Date
x.Date <- as.Date(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.Date)
plot(x)
## or POSIXct
y.POSIXct <- ISOdatetime(2003, 02, c(1, 3, 7, 9, 14), 0, 0, 0)
y <- zoo(rnorm(5), y.POSIXct)
plot(y)
## or numeric
z <- zoo(rnorm(5), runif(5))
plot(z)


# create a constant series
z <- zoo(1, seq(4)[-2])

# create a series with zero columns
z0 <- zoo(, 1:4)[,-1]
}
\keyword{ts}
