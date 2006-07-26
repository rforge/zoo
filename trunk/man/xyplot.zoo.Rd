\name{xyplot.zoo}
\alias{xyplot.zoo}
\alias{xyplot.ts}
\alias{xyplot.its}
\title{ Plot zoo Series with Lattice }
\description{
\code{xyplot} method for \code{zoo} class. Also \code{ts} and \code{its} 
classes.
}
\usage{

\method{xyplot}{zoo}(x, data, screens = seq(length = NCOL(x)), 
    default.scales = list(y = list(relation = "free")), 
    layout = c(1, nlevels(fac)), xlab = "Index", ylab = NULL, 
    main = deparse(substitute(x)), lty = 1, lwd = 1, pch = 1, 
    type = "l", col = 1, hdg = NULL, ...) 

\method{xyplot}{ts}(x, data, screens = seq(length = NCOL(x)), 
    default.scales = list(y = list(relation = "free")), 
    layout = c(1, nlevels(fac)), xlab = "Index", ylab = NULL, 
    main = deparse(substitute(x)), lty = 1, lwd = 1, pch = 1, 
    type = "l", col = 1, hdg = NULL, ...) 

\method{xyplot}{its}(x, data, screens = seq(length = NCOL(x)), 
    default.scales = list(y = list(relation = "free")), 
    layout = c(1, nlevels(fac)), xlab = "Index", ylab = NULL, 
    main = deparse(substitute(x)), lty = 1, lwd = 1, pch = 1, 
    type = "l", col = 1, hdg = NULL, ...) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Time series of class \code{zoo}, \code{ts} or \code{its}. }
  \item{data}{ Currently not used. }
  \item{screens}{ If \code{n} panels are desired then this is an unamed
    vector with one component per column of \code{x} each of which is 
    \code{1} to \code{n} showing which panel the corresponding column of 
    \code{x} should be plotted.   If fewer components are specified than 
    columns then it is extended via recylcing.  For example, 
    \code{screens=1} would plot all series in a single panel.}
    Alternately it can be a named vector with the names corresponding
    to column names of \code{x}.  If there is one unnamed component then
    any columns not named are assigned to the panel referenced by that
    unamed component.  If \code{screens} is omitted then each column is 
    plotted in a separate panel. 
  \item{default.scales}{ The default is set so that all series have the same
    \code{X} axis but free \code{Y} axis.  See \code{xyplot} in the
    \code{Lattice} package for more information on \code{scales}.  Normally
    the user should not modify this argument.  Use the \code{lattice}
    \code{scales} argument instead. }.
  \item{layout}{ See \code{layout} in the
    \code{Lattice} package for more information on \code{layout}.
      For example, c(1,3) means one column of 3 panels and 
      c(3,1) means 3 columns of 1 panel each.
  \item{xlab}{ Character string used as the X axis label. }
  \item{ylab}{ Character string used as the Y axis label.  If there
     are multiple panels it may be a character vector the same length
     as the number of panels. }
  \item{main}{ See \code{main} in \code{Lattice} package for more information.}
  \item{lty}{ See \code{lty} in \code{Lattice} package for more information.
     May be a vector the same length as the number of series or may be
     named in which case the series column names will be matched to the
     names here.  If one element is unnamed it will be used as the default.}
  \item{lwd}{ See \code{lwd} in \code{Lattice} package for more information.
     Processing is as described above for \code{lty}.}
  \item{pch}{ See \code{pch} in \code{Lattice} package for more information.
     Processing is as described above for \code{lty} except that a list
     is also allowed in which case each element may be a scalar or a 
     vector with one element per point in the corresonding column.}
  \item{type}{ See \code{type} in \code{Lattice} package for more information.
     Processing is as described above for \code{lty}.}
  \item{col}{ See \code{col} in \code{Lattice} package for more information.
     Processing is as described above for \code{pch}.}
  \item{hdg}{ Headings used for panels.  This is a character vector the same
     length as the number of panels.  Default is to use column names for
     headers.  \code{FALSE} can be used to omit headings.}
  \item{\dots}{ Other arguments as in \code{lattice} \code{xyplot}. }
}
\details{
  Plots a \code{zoo}, \code{ts} or \code{its} object using 
  the \code{Lattice} package \code{xyplot} routine.
}
\value{
  Invisibly returns a \code{trellis} class object.  Printing this
  object using \code{print} will display it.
}
\examples{

library(lattice)

z <- zoo(cbind(a = 1:5, b = 11:15, c = 21:25))

# Plot one panel per series.
print(xyplot(z))

# same but make first (i.e. bottom) panel twice as large as others
print(xyplot(z), heights = list(c(2,1,1), units = "null"))

# Plot all in one panel.
print(xyplot(z, screens = 1))

# Plot first two columns in first panel and third column in second panel.
# Plot first series using points, second series using lines and third
#  series via overprinting both lines and points
# Use colors 1, 2 and 3 for the three series (1=black, 2=red, 3=green)
# Make 2nd (upper) panel 3x the height of the 1st (lower) panel
p <- xyplot(z, screens = c(1,1,2), type = c("p", "l", "o"), col = 1:3)
print(p, panel.height = list(y = c(1, 3), units = "null"))

}
\keyword{ts}
