\name{as.zoo}
\alias{as.zoo}
\alias{as.zoo.default}
\alias{as.zoo.ts}
\alias{as.zoo.its}
\alias{as.zoo.irts}
\alias{as.matrix.zoo}
\alias{as.vector.zoo}
\alias{as.data.frame.zoo}
\alias{as.irts.zoo}
\alias{as.its.zoo}
\title{Coercion from and to zoo}
\description{
Methods for coercing \code{"zoo"} objects to other classes and
a generic function \code{as.zoo} for coercing objects to class \code{"zoo"}.
}
\usage{
as.zoo(x, \dots)
}
\arguments{
  \item{x}{an object,}
  \item{\dots}{currently not used.}
}

\details{
\code{as.zoo} currently has a default method and methods
for \code{\link{ts}}, \code{\link[its]{its}} and \code{\link[tseries]{irts}} objects.

Methods for coercing objects of class \code{"zoo"} to other classes
currently include: \code{\link{as.matrix}}, \code{\link{as.vector}},
\code{\link{as.data.frame}}. Furthermore the two coercion functions
\code{as.its.zoo} and \code{as.irts.zoo} are provided, but work only
if the corresponding packages are attached.
}

\value{
\code{as.zoo} returns a \code{\link{zoo}} object.
}

\seealso{\code{\link{zoo}}, \code{\link{ts}}, \code{\link[its]{its}}, \code{\link[tseries]{irts}}}

\examples{
as.zoo(rnorm(5))
}
\keyword{ts}
