\name{runmean}
\alias{runmean}
\alias{runmax}
\alias{runmed}
\title{Running Means/Maximums/Medians}
\description{
Compute running means, maximums and medians of \code{"zoo"}
objects.
}
\usage{
runmean(x, k, na.pad = TRUE)
runmax(x, k, na.pad = TRUE)
runmed(x, k, na.pad = TRUE, \dots)
}
\arguments{
  \item{x}{a \code{"zoo"} object,}
  \item{k}{integer width of the running window. Must be odd
    for \code{runmed}.}  
  \item{na.pad}{logical. Add \code{NA} padding for the start 
    and end values?}  
  \item{\dots}{Further arguments passed to the \code{stats}
    function \code{\link[stats]{runmed}}.}
}

\details{
These functions are still under development and might be subject
to future changes. The function \code{runmed} masks the function
of the same name from \code{stats}. See \code{\link[stats]{runmed}}.
}

\value{
A \code{"zoo"} object with the running mean/max/median.
}

\seealso{\code{\link{zoo}}}

\examples{
x.Date <- as.Date(paste("2004", rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
x <- zoo(rnorm(12), x.Date)

runmean(x, 3)
runmax(x, 3)
runmed(x, 3)

xm <- zoo(matrix(1:12, 4, 3), x.Date[1:4])
runmean(xm, 3)
runmax(xm, 3)
runmed(xm, 3)

}
\keyword{ts}

