yy <- log(c(0.01, 0.02, 0.18, 0.29, 0.62, 0.34, 0.18,
	0.02, 0.12, 0.22, 0.35, 0.18, 0.08, 0.02, 0.01))
xx <- seq(-1, 1, length = length(yy))
dens <- lm(yy ~ xx + I(xx^2) + I(xx^3) + I(xx^4))

x <- seq(-1, 1, length = 50)
rr <- predict(dens, newdata = data.frame(xx = x))
rr <- (rr - mean(rr)) / sd(rr) * 0.5
m <- 10 * exp(rr)
Y <- rpois(length(m), m)
save(Y, m, file = "Y.RData")
