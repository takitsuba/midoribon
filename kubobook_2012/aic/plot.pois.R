source("COMMON.R")
load("data.RData")

dx <- m
ymax <- 11

plot.frame <- function(
	file,
	xlim,
	ylim,
	xlab = "y",
	width = 2.0, height = 2.0
) {
	dev.on(file, width = width, height = height)
	par(mar = c(2.4, 1.5, 0.1, 0.1), mgp = c(1.3, 0.6, 0), cex = 1.2)
	plot(
		c(), c(),
		type = "n",
		xlab = xlab, 
		ylab = "",
		yaxs = "i",
		axes = FALSE,
		xlim = xlim, ylim = ylim
	)
	axis(1)
	box()
}

add.pois <- function(lambda, lty = 2, lwd = 1, pch = 21, col = "#000000", type = "o")
{
	yy <- (m - dx):(m + dx)
	lines(
		yy, n.sample * dpois(yy, lambda),
		type = type, lty = lty, pch = pch,
		lwd = lwd, col = col
	)
	#grid()
}
add.poisG <- function(lambda) add.pois(lambda, lty = 1, lwd = 3, type = "l", col = "#00000090")

add.hist <- function(vy)
{
	hs <-hist(vy, breaks = seq(-0.5, 20.5, 1), plot = FALSE)
	lines(hs)
}

# true model
plot.frame("poisT")
add.pois(m, lty = 1, pch = 19, col = "#808080")
dev.off()

# estimated models
plot.frame("poisEj1")
mm <- mean(m.data[, j1])
add.pois(mm)
dev.off()
plot.frame("poisHEj1")
add.hist(m.data[, j1])
dev.off()
plot.frame("poisHEj1p")
add.hist(m.data[, j1])
add.poisG(mm)
dev.off()

# histograms
mm <- mean(m.data[, j1])
for (i in 1:4) {
	plot.frame(sprintf("poisH%i", i), xlab = "")
	add.hist(m.data[, vj12[i]])
	add.poisG(mm)
	dev.off()
}

