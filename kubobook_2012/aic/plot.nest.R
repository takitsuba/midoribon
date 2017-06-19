source("COMMON.R")
load("data.RData")
load("glm.RData")

s <- sample(nrow(m.x), 20)
x <- m.x[,j1][s]
y <- m.data[,j1][s]

fit0 <- glm(y ~ 1, family = poisson)
fit1 <- glm(y ~ x, family = poisson)
fit2 <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), family = poisson)

plot.glm <- function(file, fit)
{
	dev.on(file, width = 4, height = 4, top = 0.1)
	plot(x, y)
	xx <- seq(min(x), max(x), length = 100)
	yy <- rep(mean(y), length(xx))
	if (file != "glm0") {
		yy <- predict(fit, newdata = list(x = xx), type = "response")
	}
	lines(xx, yy, lwd = 2, col = "#00000040")
	if (file == "glm1") abline(h = mean(y), lty = 2)
	dev.off()
}

plot.glm("glm0", fit0)
plot.glm("glm1", fit1)
plot.glm("glm2", fit2)

