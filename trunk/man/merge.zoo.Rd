\name{merge.zoo}
\alias{merge.zoo}
\alias{rbind.zoo}
\alias{cbind.zoo}
\title{Merge Two or More zoo Objects}
\description{
Merge two zoo objects by common indexes (times), or do other
versions of database "join" operations.
}

\usage{
\method{merge}{zoo}(\dots, all = TRUE, fill = NA, suffixes = NULL,
  retclass = c("zoo", "list"))
}

\arguments{
  \item{\dots}{two or more objects of class \code{"zoo"}.}
  \item{all}{logical vector of the same length as the number of \code{"zoo"}
    objects which should be merged (otherwise expanded). All indexes
    (times) of the objects corresponding to TRUE are included, for those
    corresponding to FALSE only the indexes present in all objects are
    included. For the default \code{all = TRUE} this corresponds to 
    a union of all indexes, for \code{all = FALSE} to the intersection
    of all indexes.}
  \item{fill}{an element for filling the gaps in merged \code{"zoo"}
    objects which can arise when all = TRUE and some zoo arguments have some
    index values not in all other zoo arguments.}
  \item{suffixes}{character vector of the same length as the number of
    \code{"zoo"} objects that specifies the suffixes to be used for making
    the merged column names unique.}
  \item{retclass}{character that specifies the class of the return result,
    can be \code{"zoo"} (the default), \code{"zoo"} or \code{NULL}. For details
    see below.}
}

\details{
The \code{merge} method for \code{"zoo"} objects combines the columns
of several objects either along the union of the dates (for \code{all = TRUE}, the default),
or the intersection of their dates (for \code{all = FALSE})
filling up the created gaps with the \code{fill} pattern.

If \code{retclass} is \code{"zoo"} (the default) a single merged \code{"zoo"}
object is returned. If it is set to \code{"list"} a list of \code{"zoo}
objects is returned. If \code{retclass = NULL} then instead of returning a value it updates each
argument (if it is a variable rather than an expression) in
place so as to extend or reduce it to use the common index vector.

The indexes of different
\code{"zoo"} objects can be of different classes and are coerced to
one class in the resulting object (with a warning).

The default
\code{merge} method is synonymous to the \code{cbind} method.
The \code{rbind}
method combines the dates of the \code{"zoo"} objects (duplicate dates are
not allowed) and combines the rows of the objects.
}

\value{
An object of class \code{"zoo"}.
}

\seealso{\code{\link{zoo}}}

\examples{
## simple merging
x.date <- as.POSIXct(paste("2003-02-", c(1, 3, 7, 9, 14), sep = ""))
x <- zoo(rnorm(5), x.date)

y1 <- zoo(matrix(1:10, ncol = 2), 1:5)
y2 <- zoo(matrix(rnorm(10), ncol = 2), 3:7)

## using arguments `fill' and `suffixes'
merge(y1, y2, all = FALSE)
merge(y1, y2, all = FALSE, suffixes = c("a", "b"))
merge(y1, y2, all = TRUE)
merge(y1, y2, all = TRUE, fill = 0)

## if different index classes are merged, as in
merge(x, y1, y2, all = TRUE)
## a warning is issued and the indexes are coerced
## (not necessarily giving a satisfying result, as in this case)

## extend an irregular series to a regular one:
# create a constant series
z <- zoo(1, seq(4)[-2])
# create a 0 dimensional zoo series
z0 <- zoo(, 1:4)
# do the extension
merge(z, z0)
# same but with zero fill
merge(z, z0, fill = 0)
}
\keyword{ts}
