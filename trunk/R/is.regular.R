is.regular <- function(x)
{
  delta <- try(diff(as.numeric(index(x))))
  if(class(delta) == "try-error") FALSE
    else identical(all.equal(delta, rep.int(delta[1], length(delta))), TRUE)
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

index2char.numeric <- function(x, digits = 4, ...)
{
  freq <- 1/diff(x)
  mfreq <- round(max(freq), digits = 0)
  if(identical(all.equal(freq, as.integer(round(freq, digits = 0))), TRUE) && mfreq > 1)
    return(paste(floor(x), "(", round((x - floor(x)) * mfreq, digits = 0) + 1, ")", sep = ""))
  else
    return(as.character(round(x, digits = 4)))
}
