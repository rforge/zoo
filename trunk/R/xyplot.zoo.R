# plotpanel is an internal function used by xyplot.zoo
# Thanks to Deepayan Sarkar for fixing my original function.
plotpanel <-
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
	default.scales = list(y = list(relation = "free")), 
	layout = c(1, nlevels(fac)), xlab = "Index", ylab = NULL,
	lty = 1, lwd = 1, pch = 1, type = "l", col = 1, hdg = NULL, ...) {
	   stopifnot(require("lattice")) ## FIXME
	   x <- as.zoo(x)
	   if (length(dim(x)) < 2) x <- zoo(matrix(coredata(x),,1), time(x))
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
	   if (is.null(hdg)) {
		isnotdup <- !duplicated(screens)
		hdg <- cn[isnotdup][order(screens[isnotdup])]
	   }
	   if (!identical(hdg, FALSE)) hdg <- 
		strip.custom(factor.levels = rep(hdg, length(unique(screens))))
	   if (is.null(ylab) || length(ylab) == 1) {
		   xyplot(fo, panel = plotpanel, groups = factor(col(x)),  
			type = type, default.scales = default.scales, 
			layout = layout, xlab = xlab, ylab = ylab, pch = pch, 
			col = col, lty = lty, lwd = lwd, strip = hdg, ...)
	   } else {
		   ylab <- rep(ylab, length = length(unique(screens)))
		   xyplot(fo, panel = plotpanel, groups = factor(col(x)),  
			type = type, default.scales = default.scales, 
			layout = layout, xlab = xlab, ylab = "", pch = pch, 
			col = col, lty = lty, lwd = lwd, outer = TRUE,
			strip.left = strip.custom(horizontal = FALSE, 
				factor.levels = ylab), 
			strip = hdg, ...)
	   }

}

panel.lines.ts <- 
panel.lines.its <-
panel.lines.zoo <- function(x, ...) {
	x <- as.zoo(x)
	panel.lines(time(x), coredata(x), ...)
}

panel.points.ts <- 
panel.points.its <-
panel.points.zoo <- function(x, ...) {
	x <- as.zoo(x)
	panel.points(time(x), coredata(x), ...)
}

panel.segments.ts <- 
panel.segments.its <-
panel.segments.zoo <- function(x0, x1, ...) {
	x0 <- as.zoo(x0)
	x1 <- as.zoo(x1)
	panel.segments(time(x0), coredata(x0), time(x1), coredata(x1), ...)
}

panel.text.ts <- 
panel.text.its <-
panel.text.zoo <- function(x, ...) {
	x0 <- as.zoo(x0)
	x1 <- as.zoo(x1)
	panel.text(time(x), coredata(x), ...)
}

panel.rect.ts <- 
panel.rect.its <-
panel.rect.zoo <- function(x0, x1, ...) {
	x0 <- as.zoo(x0)
	x1 <- as.zoo(x1)
	panel.rect(time(x0), coredata(x0), time(x1), coredata(x1), ...)
}

panel.arrows.ts <- 
panel.arrows.its <-
panel.arrows.zoo <- function(x0, x1, ...) {
	x0 <- as.zoo(x0)
	x1 <- as.zoo(x1)
	panel.rect(time(x0), coredata(x0), time(x1), coredata(x1), ...)
}

panel.polygon.ts <- 
panel.polygon.its <-
panel.polygon.zoo <- function(x, ...) {
	x <- as.zoo(x)
	panel.polygon(time(x), coredata(x), ...)
}

