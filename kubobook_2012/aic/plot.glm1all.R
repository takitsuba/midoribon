source("COMMON.R")
load("data.RData")
load("glm.RData")

# The example for AIC justification
x <- m.x[,j1]
y <- m.data[,j1]
fit0 <- glm(y ~ 1, family = poisson)
fit1 <- glm(y ~ x, family = poisson)
dev.on("glm1all", width = 3, height = 3, top = 0.1)
plot(
	x, y,
	type = "n",
	xlim = c(-2, 2),
	xlab = "", ylab = ""
)
xx <- seq(min(x), max(x), length = 100)
yy <- predict(fit1, newdata = list(x = xx), type = "response")
lines(xx, yy, lwd = 2, col = "#c0c0c0")
abline(h = mean(y), lty = 2)
points(x, y)
dev.off()

