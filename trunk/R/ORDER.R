ORDER <- function(x, ...)
  UseMethod("ORDER")

ORDER.default <- function(x, ..., na.last = TRUE, decreasing = TRUE)
  order(x, ..., na.last = na.last, decreasing = decreasing)
