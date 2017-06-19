# R --vanilla!
source("COMMON.R")
source("generate.d0.R")
load("d0.RData") 

width  <- 4.0 # inch
height <- 2.7 # inch

col.d <- "#aaaaaa"

range.y <- c(-1.9, 7)
plot.d0 <- function()
{
	par(mar = c(1.5, 1.5, 0.1, 0.1), mgp = c(1.5, 0.5, 0), cex = 1.0)
	plot(
		d0$x, d0$y,
		type = "n",
		ylim = range.y,
		xlab = "",
		ylab = "",
		axes = FALSE
	)
	lines(c(0, 2), c(0, 0))
	axis(1, at = seq(0.5, 2.0, 0.5), pos = 0)
	axis(2, pos = 0)
	abline(v = 0)
}
add.points <- function()
{
	points(d0$x, d0$y)
}

# data
dev.on("plotd0", width = width, height = height)
plot.d0()
add.points()
dev.off()

# lm()
dev.on("plotd0g", width = width, height = height)
plot.d0()
fit.lm <- glm(y ~ x, data = d0)
draw.norm <- function(x)
{
	abline(v = x, col = col.d)
	m <- predict(fit.lm, newdata = data.frame(x = x), type = "response")
	sd <- sd(fit.lm$residuals)
	yy <- seq(range.y[1] - 3, range.y[2] + 3, length = 100)
	polygon(
		x - dnorm(yy, m, sd) * 0.5,
		yy,
		border = NA,
		col = col.d
	)
}
draw.norm(x = 0.5)
draw.norm(x = 1.1)
draw.norm(x = 1.7)
abline(fit.lm, lty = 2, lwd = 2)
add.points()
dev.off()

# glm()
dev.on("plotd0p", width = width, height = height, top = 0)
plot.d0()
fit.glm <- glm(y ~ x, data = d0, family = poisson)
draw.pois <- function(x)
{
	abline(v = x, col = col.d)
	lambda <- predict(fit.glm, newdata = data.frame(x = x), type = "response")
	sd <- sd(fit.glm$residuals)
	for (yy in 0:10) rect(
		x - dpois(yy, lambda) * 0.5,
		yy - 0.3,
		x,
		yy + 0.3,
		border = NA,
		col = col.d
	)
}
draw.pois(x = 0.5)
draw.pois(x = 1.1)
draw.pois(x = 1.7)
b <- fit.glm$coefficients
lines(x, exp(b[1] + b[2] * x), lty = 2, lwd = 2)
add.points()
dev.off()

