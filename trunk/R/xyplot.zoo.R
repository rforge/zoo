
library(lattice)
library(zoo)

# mypanel is an internal function used by xyplot.zoo
# Thanks to Deepayan Sarkar for fixing my original function.
mypanel <-
   function(x, y, subscripts, groups,
            col = 1,
            type = "p",
            pch = 20,
	    lty = 1,
	    lwd = 1,
            ...) {
   col <- rep(as.list(col), length = nlevels(groups))
   type <- rep(as.list(type), length = nlevels(groups))
   pch <- rep(as.list(pch), length = nlevels(groups))
   lty <- rep(as.list(lty), length = nlevels(groups))
   lwd <- rep(as.list(lwd), length = nlevels(groups))

   for(g in 1:nlevels(groups)) {
        idx <- g == groups[subscripts]
	if (any(idx)) panel.xyplot(x[idx], y[idx],
                     col = col[[g]],
                     type = type[[g]],
		     pch = pch[[g]],
		     lty = lty[[g]],
		     lwd = lwd[[g]],
                     ...)
   }
}

xyplot.its <-
xyplot.ts <-
xyplot.zoo <- function(x, data, screens = seq(length = NCOL(x)), 
	scales = list(y = list(relation = "free")), 
	layout = c(1, nlevels(fac)), xlab = "Index", ylab = NULL,
	main = deparse(substitute(x)), lty = 1, lwd = 1, pch = 1, type = "l", 
	col = 1, 
	...) {
	   x <- as.zoo(x)
	   cn <- if (is.null(colnames(x))) 
                    paste("V", seq(length = NCOL(x)), sep = "")
	          else colnames(x)
           screens <- parm(cn, screens, NROW(x), NCOL(x), 1)
	   screens <- as.factor(unlist(screens))[drop = TRUE]
	   lty <- parm(cn, lty, NROW(x), NCOL(x), 1)
	   lwd <- parm(cn, lwd, NROW(x), NCOL(x), 1)
           pch <- parm(cn, pch, NROW(x), NCOL(x), 20)
           type <- parm(cn, type, NROW(x), NCOL(x), "l")
           col <- parm(cn, col, NROW(x), NCOL(x), 1)

	   tt <- rep(time(x), NCOL(x))
	   x <- coredata(x)
	   screens <- rep(screens, length = NCOL(x))
	   fac <- factor(rep(screens, each = NROW(x)))
	   fo <- if (NCOL(x) == 1) x ~ tt else x ~ tt | fac
	   xyplot(fo, panel = mypanel, groups = factor(col(x)),  type = type,
              scales = scales, layout = layout, xlab = xlab, ylab = ylab, 
	      col = col, lty = lty, lwd = lwd, ...)
}


