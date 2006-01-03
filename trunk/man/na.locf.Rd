\name{na.locf}
\alias{na.locf}
\alias{na.locf.data.frame}
\alias{na.locf.list}
\alias{na.locf.default}
\alias{na.nocb}
\alias{na.nocb.data.frame}
\alias{na.nocb.list}
\alias{na.nocb.default}
\title{Last Observation Carried Forward / Next Observation Carried Backward}
\description{
Generic function for replacing each \code{NA} with the most recent
non-\code{NA} prior to it.
}
\usage{
na.locf(object, na.rm = TRUE, \dots)
na.nocb(object, na.rm = TRUE, \dots)
}
\arguments{
  \item{object}{an object.}
  \item{na.rm}{logical. Should leading \code{NA}s be removed?}
  \item{\dots}{further arguments passed to methods.}
}

\value{
In the case of \code(na.locf},
an object in which each \code{NA} in the input object is replaced
by the most recent non-\code{NA} prior to it.  If there are no earlier non-\code{NA}s then 
the \code{NA} is omitted (if \code{na.rm = TRUE}) or it is not replaced (if \code{na.rm = FALSE}).  
In the case of \code{na.nocb} each \code{NA} in the input object is replaced
by the next non-\code{NA} after it with \code{na.rm} having an analogous
effect.
}

\seealso{\code{\link{zoo}}}

\examples{
az <- zoo(1:6)

bz <- zoo(c(2,NA,1,4,5,2))
na.locf(bz)

cz <- zoo(c(NA,9,3,2,3,2))
na.locf(cz)
}
\keyword{ts}
