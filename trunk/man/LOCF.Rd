\name{LOCF}
\alias{LOCF}
\alias{LOCF.default}
\alias{LOCF.zoo}
\title{Last Observation Carried Forward}
\description{
Generic function for replacing each NA with the most recent
non-NA prior to it.
}
\usage{
LOCF(x, \dots)
}
\arguments{
  \item{x}{an object (typically a vector or an
    object of class \code{"zoo"}),}
  \item{\dots}{currently not used.}
}

\details{
\code{LOCF} is a generic function with a default method
and a method for \code{\link{zoo}} objects.
}

\value{
An object in which each NA in the input object is replaced
by the most recent non-NA prior to it.  If there are no earlier NAs then 
the NA is not replaced.  In multivariate \code{\link{zoo}} series each column
is processed separately.
}

\seealso{\code{\link{zoo}}}

\examples{

az <- zoo(1:6)
bz <- zoo(c(2,NA,1,4,5,2))
cz <- zoo(c(NA,9,3,2,3,2))

# Set up for regressing next period az on bz and diff(cz).

# First attempt is probably wrong since we may not want to 
# fill in missing values of the dependent variable, particularly
# ones created through the merge.  Another problem is that
# LOCF is applied after diff rather than to the raw data.

z.no <- merge(lag.az = lag(az), bz = bz, diff.cz = diff(cz))
z.no
z.no. <- LOCF(z.no)
z.no.
lm(lag.az ~., na.omit(z.no.)) 

# Second attempt is correct.  This one omits rows where the 
# next period dependent variable is NA.

z.ok <- merge(lag.az = lag(az), bz = LOCF(bz), diff.cz = diff(LOCF(cz)))
z.ok
lm(lag.az ~., na.omit(z.ok))

}
\keyword{ts}
