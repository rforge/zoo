is.regular <- function(x)
{
  delta <- as.numeric(diff(index(x)))
  identical(all.equal(delta, rep.int(delta[1], length(delta))), TRUE)
}

as.ts.zoo <- function(x)
{
  stopifnot(is.regular(x))
  ix <- index(x)
  ts(coredata(x), start = as.numeric(ix[1]), deltat = as.numeric(diff(ix))[1])
}

frequency.zoo <- function(x, ...)
{
  stopifnot(is.regular(x))
  1/(as.numeric(diff(index(x)))[1])
}

deltat.zoo <- function(x, ...)
{
  stopifnot(is.regular(x))
  as.numeric(diff(index(x)))[1]
}

cycle.zoo <- function(x, ...)
{
  zoo(coredata(cycle(as.ts.zoo(x))), index(x))
}
