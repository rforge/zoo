model.frame.AsIs <- function (formula, data = NULL, subset = NULL, 
    na.action = na.omit, drop.unused.levels = FALSE, xlev = NULL, ...) 
{
	class(formula) <- "formula"
	if (is.null(data)) data <- parent.frame()
	if (!is.list(data)) data <- as.list(data)
	.Class <- class(eval(as.list(formula)[[2]], data, environment(formula)))
	NextMethod("model.frame")
}

model.frame.zoo <- function (formula, data = NULL, subset = NULL, 
    na.action = na.omit, drop.unused.levels = FALSE, xlev = NULL, ...) 
{
	args <- as.list(attr(terms(formula), "variables"))[-1]
	args$retclass <- "list"
	args$all <- FALSE
	formula <- terms(formula)
	# attr(formula, "predvars") <- as.call(append(merge.zoo, args))
	attr(formula, "predvars") <- as.call(append(merge, args))
	if (!is.null(data)) data <- as.list(data)
	# attr(formula, "predvars1") <- as.call(append(merge.zoo, append(args[2], args[-1])))
	NextMethod("model.frame", formula = formula)
}

model.frame.ts <- function (formula, data = NULL, subset = NULL, 
    na.action = na.omit, drop.unused.levels = FALSE, xlev = NULL, ...) 
{
	args <- as.list(attr(terms(formula), "variables"))[-1]
	args$dframe <- TRUE
	formula <- terms(formula)
	attr(formula, "predvars") <- as.call(append(ts.intersect, args))
	if (!is.null(data)) data <- as.list(data)
	# attr(formula, "predvars1") <- as.call(append(ts.intersect, append(args[2], args[-1])))
	mf <- NextMethod("model.frame", formula = formula)
	attr(mf, "ts") <- 
		eval(attr(formula, "predvars"), data, environment(formula))
	mf
}

