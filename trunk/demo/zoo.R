z.ts <- ts(1:20, start = c(1950,6), freq = 12)
z <- as.zoo(z.ts)

z.lm <- lm(I(z ~ lag(z, -1)))
resid(z.lm)
fitted(z.lm)

# prediction
p <- as.zoo(ts(1:6, start = c(1960,11)))
z.p <- predict(I(z.lm), list(z = p)

as.ts(z.p)

