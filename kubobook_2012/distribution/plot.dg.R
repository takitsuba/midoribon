# R --vanilla!
source("COMMON.R")

# dnorm
plot.norm <- function(file, mean, sd, axis2 = FALSE)
{
	y <- seq(-5, 5, length = 100)
	dev.on(file, width = height, height = height, top = 0.1)
	plot(
		y, dnorm(y, mean, sd),
		type = "l",
		xlim = range(y),
		ylim = c(0, dnorm(0, 0, 1) * 1.05),
		yaxs = "i",
		axes = FALSE,
		xlab = "", ylab = ""
	)
	box()
	axis(1)
	if (axis2) axis(2)
	n <- 10
	y <- seq(1.2, 1.8, length = n)
	polygon(
		c(y[1], y, y[n]),
		c(0, dnorm(y, mean, sd), 0),
		border = NA,
		col = "#00000060"
	)
	dev.off()
}
plot.norm("dnorm1", 0, 1, axis2 = TRUE)
plot.norm("dnorm2", 0, 3)
plot.norm("dnorm3", 2, 1)

# dgamma
plot.gamma <- function(file, a, b, axis2 = FALSE)
{
	y <- seq(1.0e-6, 4.8, length = 100)
	dev.on(file, width = height, height = height, top = 0.1)
	plot(
		y, dgamma(y, a, b),
		type = "l",
		xlim = c(0, max(y)),
		ylim = c(0, 1.1),
		yaxs = "i",
		axes = FALSE,
		xlab = "", ylab = ""
	)
	abline(v = 0, lty = 3)
	box()
	axis(1)
	if (axis2) axis(2)
	n <- 10
	y <- seq(1.2, 1.8, length = n)
	polygon(
		c(y[1], y, y[n]),
		c(0, dgamma(y, a, b), 0),
		border = NA,
		col = "#00000060"
	)
	dev.off()
}
plot.gamma("dgamma1", 1, 1, axis2 = TRUE)
plot.gamma("dgamma2", 5, 5)
plot.gamma("dgamma3", 0.1, 0.1)
