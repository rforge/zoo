\name{make.par.list}
\alias{make.par.list}
\title{ Make a list from a parameter specification. }
\description{
  Process arguments.  Used by \code{plot.zoo} and \code{xyplot.zoo}.
}
\usage{
make.par.list(nams, x, n, m, def, recycle = sum(unnamed) > 0)
}
\arguments{
  \item{nams}{ Names of variables.  Character vector. }
  \item{x}{ Parameter specification.  A list or a vector. See Details. }
  \item{n}{ Number of rows. }
  \item{m}{ Number of columns.  Only uses the fact that m is 
    1 or greater than 1. }
  \item{def}{ Default value. }
  \item{recycle}{ If TRUE recycle columns to provide unspecified ones.  If FALSE
    use \code{def} to provide unspecified ones.  This only applies to entire
    columns.  Within columns recycling is always done regardless of how 
    \code{recycle} is set. Defaults to \code{TRUE} if there is at least
    one unnamed variable and defaults to \code{FALSE} if there are only
    named variables in \code{x}. }
}
\details{
  This function is currently intended for internal use.  It is currently 
  used by 
  \code{plot.zoo} and \code{xyplot.zoo} but might also be used in the future
  to create additional new plotting routines.
  It creates a new list which uses the named variables from x
  and then assigns the unnamed in order.  For the remaining variables
  assign them the default value if recycle = FALSE or recycle the
  unnamed variables if recycle = TRUE.  
}
\value{
  See Details.
}
\examples{

make.par.list(letters[1:5], 1:5, 3, 5)
suppressWarnings( make.par.list(letters[1:5], 1:4, 3, 5, 99) )
make.par.list(letters[1:5], c(d=3), 3, 5, 99)
make.par.list(letters[1:5], list(d=1:2, 99), 3, 5)
make.par.list(letters[1:5], list(d=1:2, 99, 100), 3, 5)

}
\keyword{ ts }
