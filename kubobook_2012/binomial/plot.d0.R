# R --vanilla!
source("glm.R")
d0 <- d[d$f == "C",]

width  <- 3.5 # inch
height <- 2.4 # inch

col.d <- "#00000040"

range.x <- c(7, 12)
range.y <- c(0, 8)
plot.d0 <- function(type = "p")
{
	plot(
		d0$x, d0$y,
		xlim = range.x,
		ylim = range.y,
		type = type,
		xlab = "",
		ylab = "",
		axes = FALSE
	)
	axis(1, pos = 0)
	axis(2, pos = range.x[1])
	abline(v = range.x[1])
	abline(h = 0)
}

# glm()
dev.on("plotd0b1", width = width, height = height)
par(mar = c(1.5, 1.5, 0.1, 0.1), mgp = c(2.0, 0.5, 0))
plot.d0(type = "p")
dev.off()

dev.on("plotd0b2", width = width, height = height)
par(mar = c(1.5, 1.5, 0.1, 0.1), mgp = c(2.0, 0.5, 0))
plot.d0(type = "n")
fit.glm <- glm(cbind(y, N - y) ~ x, data = d0, family = binomial)
draw.binom <- function(x)
{
	abline(v = x, col = col.d)
	p <- predict(fit.glm, newdata = data.frame(x = x), type = "response")
	sd <- sd(fit.glm$residuals)
	for (yy in 0:8) rect(
		x - dbinom(yy, 8, p) * 2,
		yy - 0.4,
		x,
		yy + 0.4,
		border = NA,
		col = col.d
	)
	points(x, 8 * p, pch = 16)
}
draw.binom(x = 8.5)
draw.binom(x = 10)
draw.binom(x = 12) 
b <- fit.glm$coefficients
x <- seq(min(d$x), max(d$x), length = 100)
lines(x, 8 * logistic(b[1] + b[2] * x), lty = 1, lwd = 2)
dev.off()

