
# create some data
set.seed(1)
x <- matrix(rnorm(40), ncol = 2, dimnames = list(NULL, c("gdp", "ipi")))
x.ts <- ts(x, start = 1950, freq = 4)
z <- as.zoo(x.ts)

# fit linear model
#  When using lags normally we want to shift the response backward 
#  to align it with the predictors like this:
z.lm <- lm(I(lag(gdp) ~ lag(gdp, -1) + ipi), data = z)
z.lm

# Residuals and Fitted values
# The I(...) can be omitted for resid and fitted with a zoo series
#  but they must be there for a ts series as well as for lm and predict 
#  so its easier just to put them in so we do not have to worry about which
#  case we are in.
resid(I(z.lm))
fitted(I(z.lm))

z.pred <- predict(I(z.lm), z)
z.pred

# Plot z[,"gdp"] lagged and concatentated with the one step prediction.
# Also overplot the one step prediction in red.
z.pred.tail <- tail(z.pred,1)
plot(merge(c(lag(z[,"gdp"]), z.pred.tail), z.pred.tail), 
	plot.type = "single", col = 1:2, type = "b", pch = 20)

# The zoo package has some ts facilities too.
# Repeat the above using ts and the facilities of the zoo package.
# When running a regression using ts and lags put the lags on the 
#   as shown right side.  This is different than with zoo class.
x.ts.lm <- lm(I(gdp ~ lag(gdp, -2) + lag(ipi,-1)), data = x.ts)

resid(I(x.ts.lm))
fitted(I(x.ts.lm))

x.pred <- predict(I(x.ts.lm), x.ts)
x.pred

ts.plot( x.ts[,"gdp"], tail(x.pred, 1), 
	gpars = list(type = "b", col = 1:2, pch = 20))


