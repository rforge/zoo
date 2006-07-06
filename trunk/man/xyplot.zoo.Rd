\name{xyplot.zoo}
\alias{xyplot.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{xyplot}{zoo}(x, data, screens = seq(length = NCOL(x)), scales = list(y = list(relation = "free")), layout = c(1, nlevels(fac)), xlab = "Index", ylab = NULL, ...)
\method{xyplot}{ts}(x, data, screens = seq(length = NCOL(x)), scales = list(y = list(relation = "free")), layout = c(1, nlevels(fac)), xlab = "Index", ylab = NULL, ...)
\method{xyplot}{its}(x, data, screens = seq(length = NCOL(x)), scales = list(y = list(relation = "free")), layout = c(1, nlevels(fac)), xlab = "Index", ylab = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{screens}{ ~~Describe \code{screens} here~~ }
  \item{scales}{ ~~Describe \code{scales} here~~ }
  \item{layout}{ ~~Describe \code{layout} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

z <- zoo(matrix(1:24, 8), as.Date("2000-01-01") + 0:7)
print(xyplot(z))
print(xyplot(z, screens = 1))
print(xyplot(z, screens = c(1,1,2), type = c("p", "l", "o"), col = 1:3))

}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
