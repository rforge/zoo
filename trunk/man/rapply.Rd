\name{rapply}
\alias{rapply}
\alias{rapply.zoo}
\alias{rollmean}
\alias{rollmean.zoo}
\alias{rollmed}
\alias{rollmed.zoo}
\alias{rollmax}
\alias{rollmax.zoo}
\title{Apply Rolling Functions}
\description{
  Returns a vector or array or list of values obtained by applying a
  function to margins of an array.
}
\usage{
rapply(data, width, FUN, MARGIN = 1, by.column = TRUE, na.pad = TRUE, \dots)
}
\arguments{
  \item{data}{the data to be used.  Currently only \code{zoo} objects are
   supported.}
  \item{width}{number of points per group.}
  \item{FUN}{the function to be applied.
    In the case of functions like \code{+}, \code{\%*\%}, etc., the
    function name must be quoted.}
  \item{MARGIN}{a vector giving the subscripts which the function will
    be applied over.
    \code{1} indicates rows, \code{2} indicates columns,
    \code{c(1,2)} indicates rows and columns. Currently only \code{1} is
    supported.}
  \item{by.column}{if \code{TRUE} and \code{MARGIN} is \code{1} 
    then \code{FUN} is applied to each column separately.}
  \item{na.pad}{if \code{TRUE} and \code{MARGIN} is \code{1} 
    then additional elements or rows of \code{NA}s are added so that 
    result has same number of elements or rows as \code{data}.}
  \item{\dots}{optional arguments to \code{FUN}.}
}
\details{
  Groups time points in successive sets of \code{width} time points and 
  applies \code{FUN} to the corresponding values.   If \code{FUN} is
  \code{mean}, \code{max} or \code{median} and \code{by.column} is 
  \code{TRUE} then special purpose code is used to enhance performance.
}
\value{
  A \code{zoo} object.
}
\examples{
z <- zoo(11:15, as.Date(31:35)) 
rapply(z, 2, mean)
zm <- zoo(matrix(1:12, 4), as.Date(31:34))
rapply(zm, 2, mean)

}
\keyword{iteration}
\keyword{array}
