#Z# Note, that this code is under development.

# modified version of stats::lm
dynlm <- function (formula, data = list(), subset, weights, na.action, 
    method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, 
    singular.ok = TRUE, contrasts = NULL, offset = NULL, preserve.data = TRUE, ...) 
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf$singular.ok <- mf$model <- mf$method <- NULL
    mf$x <- mf$y <- mf$qr <- mf$contrasts <- mf$... <- NULL
    mf$drop.unused.levels <- TRUE

    ### next line is only changed line

    mf[[1]] <- as.name("dynlm.model.frame")
    mf <- eval(mf, parent.frame())
    if (method == "model.frame") 
        return(mf)
    else if (method != "qr") 
        warning("method = ", method, " is not supported. Using \"qr\".")
    mt <- attr(mf, "terms")
    na.act <- attr(mf, "na.action")
    xvars <- as.character(attr(mt, "variables"))[-1]
    if ((yvar <- attr(mt, "response")) > 0) 
        xvars <- xvars[-yvar]
    xlev <- if (length(xvars) > 0) {
        xlev <- lapply(mf[xvars], levels)
        xlev[!sapply(xlev, is.null)]
    }
    if (!singular.ok) 
        warning("only `singular.ok = TRUE' is currently implemented.")
    y <- model.response(mf, "numeric")
    w <- model.weights(mf)
    offset <- model.offset(mf)
    if (!is.null(offset) && length(offset) != NROW(y)) 
        stop("Number of offsets is ", length(offset), ", should equal ", 
            NROW(y), " (number of observations)")
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = numeric(0), residuals = y, fitted.values = 0 * 
            y, weights = w, rank = 0, df.residual = length(y))
        if (!is.null(offset)) 
            z$fitted.values <- offset
    }
    else {
        x <- model.matrix(mt, mf, contrasts)
        z <- if (is.null(w)) 
            lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
                ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
            ...)
    }
    class(z) <- c("dynlm", c(if (is.matrix(y)) "mlm", "lm"))
    if (!is.null(na.act)) 
        z$na.action <- na.act
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- xlev
    z$call <- cl
    z$terms <- mt
    if (model) 
        z$model <- mf
    if (ret.x) 
        z$x <- x
    if (ret.y) 
        z$y <- y
	if(preserve.data) {
                #Z# getlmVars (or something similar)
		#Z# could become an independent function
		#Z# but is currently only needed here
		
		getlmVars <- function(formula) {
			env <- environment(formula)
			data <- environment(formula)
			formula <- terms(formula, data = data)
			vars <- attr(formula, "variables")
			predvars <- vars
			eval(predvars, data, env)
		}

		z$original.data <- getlmVars(formula)
		
	}
    z
}

# modified version of stats:::model.frame.default
dynlm.model.frame <- function (formula, data = NULL, subset = NULL, na.action = na.omit,
    drop.unused.levels = FALSE, xlev = NULL, ...) 
{
    if (missing(formula)) {
        if (!missing(data) && inherits(data, "data.frame") && 
            length(attr(data, "terms")) > 0) 
            return(data)
        formula <- as.formula(data)
    }
    else if (missing(data) && inherits(formula, "data.frame")) {
        if (length(attr(formula, "terms"))) 
            return(formula)
        data <- formula
        formula <- as.formula(data)
    }
    if (missing(na.action)) {
        if (!is.null(naa <- attr(data, "na.action")) & mode(naa) != 
            "numeric") 
            na.action <- naa
        else if (!is.null(naa <- getOption("na.action"))) 
            na.action <- naa
    }
    if (missing(data)) 
        data <- environment(formula)
    else if (!is.data.frame(data) && !is.environment(data) && 
        !is.null(attr(data, "class"))) 
        data <- as.data.frame(data)
    else if (is.array(data)) 
        stop("`data' must be a data.frame, not a matrix or  array")
    env <- environment(formula)
    if (!inherits(formula, "terms")) 
        formula <- terms(formula, data = data)
    rownames <- attr(data, "row.names")
    vars <- attr(formula, "variables")
    predvars <- attr(formula, "predvars")
    if (is.null(predvars)) 
        predvars <- vars
    varnames <- as.character(vars[-1])
    variables <- eval(predvars, data, env)
	
	#######################	
	#######################	
	#######################	
	# this is the hack
	variables$retclass = "list"
	variables$all = FALSE
	variables <- do.call("merge.zoo", variables)
	#######################	
	#######################	
	#######################	   
    
    if (is.null(attr(formula, "predvars"))) {
        for (i in seq(along = varnames)) predvars[[i + 1]] <- makepredictcall(variables[[i]], 
            vars[[i + 1]])
        attr(formula, "predvars") <- predvars
    }
    extranames <- names(substitute(list(...))[-1])
    extras <- substitute(list(...))
    extras <- eval(extras, data, env)
    subset <- eval(substitute(subset), data, env)
    
    ########################
	########################
	########################
	########################
	# this is the part that kills the NA rows
    data <- .Internal(model.frame(formula, rownames, variables, varnames, extras, extranames, subset, na.action))
    
    
    if (length(xlev) > 0) {
        for (nm in names(xlev)) if (!is.null(xl <- xlev[[nm]])) {
            xi <- data[[nm]]
            if (is.null(nxl <- levels(xi))) 
                warning(paste("variable", nm, "is not a factor"))
            else {
                xi <- xi[, drop = TRUE]
                if (any(m <- is.na(match(nxl, xl)))) 
                  stop(paste("factor", nm, "has new level(s)", 
                    nxl[m]))
                data[[nm]] <- factor(xi, levels = xl)
            }
        }
    }
    else if (drop.unused.levels) {
        for (nm in names(data)) {
            x <- data[[nm]]
            if (is.factor(x) && length(unique(x)) < length(levels(x))) 
                data[[nm]] <- data[[nm]][, drop = TRUE]
        }
    }
    data
}

#Z# new model.frame function by Gabor in revision l
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
	attr(formula, "predvars") <- as.call(append(merge.zoo, args))
	NextMethod("model.frame", formula = formula)
}
