is.regular <- function(x) {
  UseMethod("is.regular")
}

is.regular.zoo <- function(x)
{
  delta <- try(diff(as.numeric(index(x))))
  if(class(delta) == "try-error") FALSE
    else identical(all.equal(delta, rep.int(delta[1], length(delta))), TRUE)
}

is.regular.ts <- function(x) {
  return(TRUE)
}

is.regular.default <- function(x) {
  is.regular(as.zoo(x))
}


frequency.zoo <- function(x, ts.eps = getOption("ts.eps"), ...)
{
  d <- try(diff(as.numeric(index(x))))
  reg <- if(class(d) == "try-error") FALSE
    else identical(all.equal(d, rep.int(d[1], length(d))), TRUE)
  if(!reg) warning("`x' is not a regular series")
  deltat <- min(d)
  frequency <- 1/deltat
  if(frequency > 1 && identical(all.equal(frequency, round(frequency)), TRUE))
    frequency <- round(frequency)
  if(!identical(all.equal(frequency*d, round(frequency*d)), TRUE))
    warning("`x' does not have an underlying frequency")
  return(frequency)
}

deltat.zoo <- function(x, ...)
{
  1/frequency.zoo(x, ...)
}

as.ts.zoo <- function(x, frequency = NULL, ...)
{
  d <- try(diff(as.numeric(index(x))))
  reg <- if(class(d) == "try-error") FALSE
    else identical(all.equal(d, rep.int(d[1], length(d))), TRUE)
  deltat <- min(d)
  freq <- if(is.null(frequency)) 1/deltat else frequency
  
  if(reg) {
    return(ts(coredata(x), start = as.numeric(index(x)[1]), frequency = freq))
  } else {
    if(freq > 1 && identical(all.equal(freq, round(freq, digits = 0)), TRUE))
      freq <- round(freq, digits = 0)
    if(!identical(all.equal(freq*d, round(freq*d, digits = 0)), TRUE)) {
      warning("`x' does not have an underlying regularity")
      if(is.null(frequency)) freq <- 1
      return(ts(coredata(x), frequency = freq))
    } else {
      round. <- function(x) deltat * round(x/deltat, digits = 0)
      tt <- round.(time(x))
      tt2 <- round.(seq(head(tt,1), tail(tt,1), deltat))
      xx <- merge(zoo(coredata(x), tt), zoo(, tt2))
      return(ts(coredata(xx), start = as.numeric(index(x)[1]), frequency = freq))
    }
  }
}

cycle.zoo <- function(x, ...)
{
  zoo(coredata(cycle(as.ts.zoo(x))), index(x))
}

index2char.numeric <- function(x, digits = 4, ...)
{
  if(length(x) < 2) return(as.character(round(x, digits = digits)))

  d <- diff(x)
  deltat <- min(d)
  freq <- 1/deltat
  if(freq > 1 && identical(all.equal(freq, round(freq, digits = 0)), TRUE)) freq <- round(freq, digits = 0)
  if(identical(all.equal(freq*d, round(freq*d, digits = 0)), TRUE))
    return(paste(floor(x), "(", round((x - floor(x)) * freq, digits = 0) + 1, ")", sep = ""))
  else
    return(as.character(round(x, digits = digits)))
}
