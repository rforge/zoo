\name{as.yearmon}
\alias{as.yearmon}
\alias{as.yearmon.numeric}
\alias{as.yearmon.integer}
\alias{as.yearmon.default} 
\alias{as.Date.yearmon}
\alias{format.yearmon}
\alias{as.character.yearmon}
\alias{print.yearmon}
\alias{[.yearmon}
\alias{axis.yearmon}
\title{ Coerce to yearmon class }
\description{
  yearmon class represents monthly data
}
\usage{
as.yearmon(x)
}
\arguments{
  \item{x}{ a numeric, integer, Date or object that is coercable to POSIXlt }
}
\details{
  The yearmon class is used to represent monthly data.  Internally it holds
  the data as year plus 0 for January, 1/12 for February, 2/12 for March
  and so on in order that its internal representation is the same as 
  \code{ts} class with \code{frequency = 12}.
}
\value{
  Returns its argument converted to class \code{yearmon}.
}

\seealso{ See Also \code{\link{as.ts.zoo}} }
\examples{

as.yearmon(1950 + seq(0, 23)/12)

}





