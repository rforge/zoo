

as.zoo.tis <- function(x, ...) {
	as.zoo(as.zooreg(x, ...))
}

as.zoo.tis <- function(x, class = "ti", ...) {
	require(fame)
	if (class == "ti") {
		as.zoo(as.zooreg(x, class = "ti", ...))
	} else if (class == "numeric") {
		zoo(stripTis(x), time(x))
    } else {
		asFun <- paste("as", class, sep = ".")
		if (class == "numeric") asFun <- force
		zoo(stripTis(x), 
			do.call(asFun, list(POSIXct(time(x), tz = "GMT"))), ...)
	}
}

as.zooreg.tis <- function(x, class = "ti", ...) {
	require(fame)
	if (class == "ti")
		zooreg(stripTis(x), start = start(x), ...)
	else 
		as.zooreg(as.zoo(x, class = class, ...))
}

as.tis.zoo <- function(x, ...) {
	local.as.ts.zoo <- function (x, ...){
	 if(is.regular(x)){
	   if(is.ti(firstIndex <- index(x)[1])){
			 xts <- ts(unclass(x),
				   start = c(year(firstIndex), cycle(firstIndex)),
				   freq = frequency(firstIndex))
		 attr(xts, "index") <- NULL
		 return(xts)
	   }
	   else {
		 attr(x, "frequency") <- frequency(x)
		 return(as.ts(x))
	  }
	 }
	 else{
	   warning(paste(sQuote("x"), "does not have an underlying regularity"))
	   return(ts(coredata(x)))
	 }
	}
	as.tis(local.as.ts.zoo(x), ...)
}

as.tis.zoo <- function(x, frequency, ...){

  if(is.regular(x)){
    if(is.ti(firstIndex <- index(x)[1])){

      xtis <- ti(unclass(x), start = firstIndex)
      attr(xtis, "index") <- NULL
          return(xtis)
        }

        else {
		  if (missing(frequency)) {
			firstIndex <- index(x)[1]
			if (inherits(firstIndex, "Date")) frequency <- 365
			else if (inherits(firstIndex, "yearmon")) frequency <- 12
			else if (inherits(firstIndex, "yearqtr")) frequency <- 4
			else frequency <- frequency(x)
		  }
          attr(x, "frequency") <- frequency(x)
          return(as.tis(as.ts(x)))
        }
  }
  else{
    warning(paste(sQuote("x"), "does not have an underlying regularity"))
    return(as.tis(ts(coredata(x))))
  }
 }


