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
x.date <- as.POSIXct(paste("2003-", rep(1:4, 4:1), "-", sample(1:28, 10, replace = TRUE), sep = ""))
x <- zoo(rnorm(12), x.date)

runmean(x, 5)
runmax(x, 5)
runmed(x, 5)
}
\keyword{ts}
