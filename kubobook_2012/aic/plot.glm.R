source("COMMON.R")
d <- read.csv("../poisson/data3a.csv")

plot.glm <- function(x, y, file, fit)
{
	dev.on(file, width = 3, height = 3, top = 0.1)
	plot(x, y, xlab = "", ylab = "", type = "n")
	xx <- seq(min(x), max(x), length = 100)
	yy <- rep(mean(y), length(xx))
	if (file != "glm0") {
		yy <- predict(fit, newdata = list(x = xx), type = "response")
	}
	lines(xx, yy, lwd = 3, col = "#c0c0c0")
	points(x, y)
	dev.off()
}

# 20 points
fit0 <- glm(y ~ 1, family = poisson, data = d)
fit2 <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), family = poisson, data = d)

plot.glm(d$x, d$y, "glm0", fit0)
plot.glm(d$x, d$y, "glm2", fit2)

