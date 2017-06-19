source("COMMON.R")

x <- seq(-7.5, 7.5, 0.2)
plot.frame <- function(file) {
	dev.on(file, width = 4, height = 2)
	par(mar = c(1.5, 0.1, 0.1, 0.1)) # margin (bottom, left, top, right)
	par(mgp = c(1.5, 0.5, 0.0))
	par(xaxs = "i", yaxs = "i")
	plot(
		numeric(0), numeric(0), type = "n",
		xlim = range(x),
		ylim = c(0, dnorm(0, 0, 1)),
		xlab = "",
		ylab = "",
		axes = FALSE
	)
	abline(h = 0)
	abline(v = 0, lty = 2, lwd = 2)
	axis(1)
}

lines.norm <- function(sd, col, xy)
{
	lines(
		x,
		dnorm(x, 0, sd),
		lwd = 2,
		col = col
	)
###	text(
###		xy[1], xy[2] * 0.98,
###		labels = expression(italic(s)),
###		pos = 4,
###		col = col
###	)
###	text(
###		xy[1] + 0.5, xy[2],
###		labels = sprintf("= %g", sd),
###		pos = 4,
###		col = col
###	)
}

plot.frame("r")
lines.norm(1.0, "#777777", c(0.3, dnorm(0.3, 0, 1)))
lines.norm(1.5, "#000000", c(1.5, dnorm(1.5, 0, 2)))
lines.norm(3.0, "#777777", c(3.2, dnorm(3.0, 0, 3)))
dev.off()

d <- read.csv("data6b.csv")
plot.frame("r0")
lines.norm(2.5, "#000000", c(1.5, dnorm(1.5, 0, 2)))
points(d$re, rep(0, nrow(d)), pch = "I", cex = 2, col = "#00000080")
dev.off()


