\name{na.approx}
\alias{na.approx}
\alias{na.approx.default}
\title{Replace NA by Interpolation}
\description{
Generic function for replacing each \code{NA} with interpolated
values.
}
\usage{
na.approx(y, x, FUN, na.rm = TRUE, \dots) 
}
\arguments{
  \item{y}{variable for which \code{NA}s are to be replaced}
  \item{x}{variable to use for interpolation. If \code{FUN} is specified then
   it defaults to \code{FUN(time(y))}; otherwise, it defaults to 1, 2, 3, \dots .}
  \item{FUN}{function to transform time into x.  May be omitted.}
  \item{na.rm}{logical. Should leading \code{NA}s be removed?}
  \item{\dots}{further arguments passed to methods.}
}

\value{
An object in which each \code{NA} in the input object is replaced
by interpolating the non-\code{NA} values before and after it.
Leading \code{NA}s are omitted (if \code{na.rm = TRUE}) or not replaced (if \code{na.rm = FALSE}). 
}

\seealso{\code{\link{zoo}}}

\examples{

z <- zoo(c(2,NA,1,4,5,2), c(1,3,4,6,7,8))
na.approx(z) 
na.approx(z, 1:6) # same
na.approx(z, time(z))
na.approx(z, FUN = as.numeric) # same

na.approx(zoo(c(NA,9,3,2,3,2)))

}
\keyword{ts}
