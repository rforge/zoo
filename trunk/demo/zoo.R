
# create a ts series, convert it to zoo
set.seed(1)
z.ts <- ts(1:20 + rnorm(20), start = c(1950,6), freq = 12)
z <- as.zoo(z.ts)

# regress z on its lag and calc residual and fitted
# I() allows auto merging/aligning of columns
z.lm <- lm(I(z ~ lag(z, -1) + lag(z, -2))) 
resid(z.lm)
fitted(z.lm)

# prediction - I forces result to be zoo
p <- as.zoo(ts(1:6, start = c(1960,11)))
z.p <- predict(I(z.lm), list(z = p))

as.ts(z.p)

