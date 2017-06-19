# R --vanilla!
source("COMMON.R")

v.x <- seq(-5, 5, length = 200)
s.max <- 0.5
y.max <- dnorm(0, 0, s.max)

pp <- function(v.x, v.y, col = "#c0c0c0")
{
	polygon(
		c(min(v.x), v.x, max(v.x)),
		c(0, v.y, 0),
		border = NA,
		col = col
	)
}

plot.prior <- function(title, v.y)
{
	file <- paste("priorexample", title, sep = "")
	dev.on(file, width = 2.5, height = 2.5)
	par(mar = c(2.0, 0.5, 0.1, 0.5), xpd = TRUE)
	plot(
		v.x, rep(0, length(v.x)),
		type = "n",
		lty = 2,
		xlab = "", ylab = "",
		ylim = c(0, y.max),
		xaxs = "i", yaxs = "i",
		axes = FALSE
	)
	if (!is.na(v.y[1])) pp(v.x, v.y)
	axis(1)
	abline(h = 0, lwd = 2)
}

# priors
plot.prior("A", dnorm(v.x, 0, 1))
dev.off()
plot.prior("B", dunif(v.x, min(v.x), max(v.x)))
dev.off()
plot.prior("C", NA)
hcol <- "#00000020"
pp(v.x, dnorm(v.x, 0, s.max * 4), col = hcol)
pp(v.x, dnorm(v.x, 0, s.max * 2), col = hcol)
pp(v.x, dnorm(v.x, 0, s.max), col = hcol)
dev.off()

