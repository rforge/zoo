index2char <- function(x, ...) UseMethod("index2char")

index2char.default <- function(x, ...) as.character(x)

index2char.numeric <- function(x, frequency = NULL, digits = 4, ...)
{
  freq <- frequency
  d <- diff(x)
  if(is.null(freq)) {
    if(length(x) < 3) return(as.character(round(x, digits = digits)))
    deltat <- min(d)
    freq <- 1/deltat
    if(freq > 1 && identical(all.equal(freq, round(freq)), TRUE)) freq <- round(freq)
  }
  if(identical(all.equal(freq*d, round(freq*d)), TRUE)) {
    if(freq == 1) return(as.character(round(x)))
      else return(paste(floor(x), "(", round((x - floor(x)) * freq) + 1, ")", sep = ""))
  } else
    return(as.character(round(x, digits = digits)))
}
