\name{na.locf}
\alias{na.locf}
\alias{na.locf.data.frame}
\alias{na.locf.list}
\alias{na.locf.default}
\title{Last Observation Carried Forward}
\description{
Generic function for replacing each \code{NA} with the most recent
non-\code{NA} prior to it.
}
\usage{
na.locf(object, na.rm = TRUE, \dots)
\method{na.locf}{default}(object, na.rm = TRUE, fromLast, rev, \dots)
}
\arguments{
  \item{object}{an object.}
  \item{na.rm}{logical. Should leading \code{NA}s be removed?}
  \item{fromLast}{logical. Causes observations to be carried backward rather
    than forward.  Default is \code{FALSE}.
    This corresponds to NOCB (next observation carried backward).}
  \item{rev}{Use fromLast instead.  This argument will
    be eliminated in the future in favor of fromLast.}
  \item{\dots}{further arguments passed to methods.}
}

\value{
An object in which each \code{NA} in the input object is replaced
by the most recent non-\code{NA} prior to it.  If there are no earlier non-\code{NA}s then 
the \code{NA} is omitted (if \code{na.rm = TRUE}) or it is not replaced (if \code{na.rm = FALSE}).  
}

\seealso{\code{\link{zoo}}}

\examples{
az <- zoo(1:6)

bz <- zoo(c(2,NA,1,4,5,2))
na.locf(bz)
na.locf(bz, fromLast = TRUE)

cz <- zoo(c(NA,9,3,2,3,2))
na.locf(cz)

}
\keyword{ts}
