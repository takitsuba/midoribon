source("COMMON.R")

set.seed(101)

x <- seq(-7.5, 7.5, 0.05)
xb <- 0:8
xb.max <- max(xb)

plot.binom <- function(file, re)
{
	ymax <- length(re) * dbinom(xb.max * 0.5, xb.max, 0.5) * 1.1
	dev.on(file, width = 2.8, height = 2.1, top = 0.5)
	y <- rbinom(length(re), max(xb), prob = 1 / (1 + exp(-re)))
	cat(sprintf("sd = %.1f s.var = %.1f\n", sd, var(y)))
	plot(
		xb,
		as.numeric(table(factor(y, levels = xb))),
		ylim = c(0, ymax),
		xlab = "",
		ylab = "",
		axes = FALSE
	)
	axis(1)
	axis(2)
	lines(
		xb, dbinom(xb, max(xb), 0.5) * length(re),
		type = "b",
		pch = 20,
		cex = 1.0,
		col = "#00000080"
	)
}
plot.frame <- function(file, ymax)
{
	dev.on(file, width = 2.8, height = 1.4)
	par(mar = c(1.5, 0.1, 0.1, 0.1)) # margin (bottom, left, top, right)
	par(mgp = c(1.5, 0.5, 0.0))
	par(xaxs = "i", yaxs = "i")
	plot(
		numeric(0), numeric(0), type = "n",
		xlim = range(x),
		ylim = c(0, ymax),
		xlab = "",
		ylab = "",
		axes = FALSE
	)
	abline(h = 0)
	abline(v = 0, lty = 2, lwd = 2)
	axis(1)
}

lines.norm <- function(sd, col, re)
{
	lines(
		x,
		dnorm(x, 0, sd),
		lwd = 2,
		col = col
	)
	points(re, rep(0, length(re)), pch = "I", cex = 2, col = "#00000040")
}

n <- 50
v.sd <- c(0.5, 3)
sink("rsim.sd.txt")
for (i in 1:length(v.sd)) {
	sd <- v.sd[i]
	re <- rnorm(n, 0, sd)
	plot.frame(sprintf("rsim%ia", i), ymax = dnorm(0, 0, v.sd[1]))
	lines.norm(sd, "#808080", re)
	dev.off()
	plot.binom(sprintf("rsim%ib", i), re)
	dev.off()
}
sink()

