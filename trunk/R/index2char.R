index2char <- function(x, ...) UseMethod("index2char")

index2char.default <- function(x, ...) as.character(x)

index2char.numeric <- function(x, digits = 4, ...)
{
  if(length(x) < 2) return(as.character(round(x, digits = digits)))
  d <- diff(x)
  deltat <- min(d)
  freq <- 1/deltat
  if(freq > 1 && identical(all.equal(freq, round(freq)), TRUE)) freq <- round(freq)
  if(identical(all.equal(freq*d, round(freq*d)), TRUE)) {
    if(freq == 1) return(as.character(round(x)))
      else return(paste(floor(x), "(", round((x - floor(x)) * freq) + 1, ")", sep = ""))
  } else
    return(as.character(round(x, digits = digits)))
}
