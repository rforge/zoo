\name{as.yearqtr}
\alias{as.yearqtr}
\alias{as.yearqtr.numeric}
\alias{as.yearqtr.integer}
\alias{as.yearqtr.default} 
\alias{c.yearqtr}
\alias{as.Date.yearqtr}
\alias{format.yearqtr}
\alias{as.character.yearqtr}
\alias{print.yearqtr}
\alias{[.yearqtr}
\alias{axis.yearqtr}
\alias{as.POSIXct.yearqtr}
\alias{as.POSIXlt.yearqtr}
\alias{as.yearqtr.yearmon}
\alias{as.yearqtr.yearqtr}
\alias{MATCH.yearqtr}

\title{ Coerce to yearqtr class }
\description{
  yearqtr class represents monthly data
}
\usage{
as.yearqtr(x, ...)
}
\arguments{
  \item{x}{ a numeric, integer, Date or object that is coercable to POSIXlt }
  \item{\dots}{ other arguments }
}
\details{
  The yearqtr class is used to represent quaterly data.  Internally it holds
  the data as year plus 0 for Q1, 1/4 for Q2, 1/2 for Q3 and 3/4 for Q4
  in order that its internal representation is the same as 
  \code{ts} class with \code{frequency = 4}.
}
\value{
  Returns its argument converted to class \code{yearqtr}.
}

\examples{

as.yearqtr(1950 + seq(0, 7)/4)

}
\keyword{ts}
