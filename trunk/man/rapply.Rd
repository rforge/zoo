\name{rapply}
\alias{rapply}
\alias{rapply.zoo}
\title{Apply Rolling Functions}
\description{
  A generic function for applying a function to rolling margins of an array.
}
\usage{
rapply(data, width, FUN, by = 1, ascending = TRUE, by.column = TRUE, na.pad = FALSE, \dots)
}
\arguments{
  \item{data}{the data to be used.  Currently only \code{zoo} objects are
   supported.}
  \item{width}{number of points per group.}
  \item{FUN}{the function to be applied.
    In the case of functions like \code{+}, \code{\%*\%}, etc., the
    function name must be quoted.}
  \item{by}{calculate FUN for trailing width points at every by-th time
    point.}
  \item{ascending}{if TRUE then points are passed to \code{FUN} in
    ascending order of time; otherwise, they are passed in descending order.}
  \item{by.column}{if \code{TRUE} 
    \code{FUN} is applied to each column separately.}
  \item{na.pad}{if \code{TRUE} 
    then additional elements or rows of \code{NA}s are added so that 
    result has same number of elements or rows as \code{data}.}
  \item{\dots}{optional arguments to \code{FUN}.}
}
\details{
  Groups time points in successive sets of \code{width} time points and 
  applies \code{FUN} to the corresponding values.   If \code{FUN} is
  \code{mean}, \code{max} or \code{median} and \code{by.column} is 
  \code{TRUE} and there are no extra arguments
  then special purpose code is used to enhance performance.
  See \code{\link{rollmean}}, \code{\link{rollmax}} and \code{\link{rollmed}}
  for more details.
}
\value{
  A \code{zoo} object.
}

\seealso{\code{\link{rollmean}}, \code{\link{rollmax}}, \code{\link{rollmed}}}

\examples{
z <- zoo(11:15, as.Date(31:35)) 
rapply(z, 2, mean)

z2 <- zoo(rnorm(6))
rapply(z2, 3, mean, by = 3) # means of nonoverlapping groups of 3
aggregate(z2, c(3,3,3,6,6,6), mean) # same

rapply(z2, 3, mean) # uses rollmean which is optimized for mean
rollmean(z2, 3) # same
rapply(z2, 3, (mean)) # does not use rollmean
}
\keyword{iteration}
\keyword{array}
