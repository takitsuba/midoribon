source("../COMMON.R")
load("d.RData")

b1 <- 1.5
b2 <- 0.1
xlim <- c(-2, 2) + 5

generate.data <- function(n = 20)
{
	d <- data.frame(x = seq(xlim[1], xlim[2], length = n))
	d$y <- rpois(n, exp(b1 + b2 * d$x))
	file <- "d.RData"
	cat("# output to", file, "...\n")
	save(d, file = file)
}

plot.frame <- function(file, type = "n", width = 2, height = 2)
{
	dev.on(file = file, width = width, height = height, top = 0.1)
	plot(
		d$x, d$y,
		type = "n", xlab = "", ylab = ""
	)
}
add.xy <- function(pch = 1, ...)
{
	points(d$x, d$y, pch = pch, ...)
}
add.mean <- function(bb1, bb2, lty = 2, lwd = 1, ...)
{
	lines(d$x, exp(bb1 + bb2 * (d$x - mean(d$x))), lty = lty, lwd = lwd, ...)
}
add.original <- function(...) add.mean(b1 + b2 * mean(d$x), b2, lty = 2, lwd = 1, ...)
