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
  \item{y}{variable for which NAs are to be replaced}
  \item{na.rm}{logical. Should leading \code{NA}s be removed?}
  \item{x}{variable to use for interpolation.   If FUN is specified then
   it defaults to FUN(time(y)) ; otherwise, it defaults to 1, 2, 3, \dots .}
  \item{FUN}{FUN to transform time into x.  May be omitted.}
  \item{\dots}{currently not used.}
}

\value{
An object in which each \code{NA} in the input object is replaced
by interpolating the non-\code{NA} values before and after it.
the \code{NA} is omitted (if \code{na.rm = TRUE}) or it is not replaced (if \code{na.rm = FALSE}). 
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
