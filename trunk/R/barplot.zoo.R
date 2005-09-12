
barplot.zoo <- function(height, names = time(height), beside = TRUE, ...) 
	barplot(t(height), names = names, beside = beside, ...)

