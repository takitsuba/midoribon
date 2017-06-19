source("./COMMON.R")

width  <- 3.0 # inch
height <- 3.0 # inch

plot.poly <- function(v.x, v.y) polygon(
	c(min(v.x), v.x, max(v.x)),
	c(0, v.y, 0), # assuming mu = 0!
	border = NA,
	col = "#c0c0c0"
)

# tau, non-informative prior
tau <- exp(seq(log(1.0E-3), log(10), length = 100))
g <- c(1, 1.0E-2, 1.0E-4)
ylim <- c(0, 1.0)
for (i in 1:length(g)) {
	y <- dgamma(tau, g[i], g[i])
	dev.on(sprintf("gamma%i", i), width = width, height = height)
	par(mar = c(2.5, 1.5, 0.5, 0.5))
	plot(
		tau, y, type = "n",
		ylim = ylim,
		xlab = "tau",
		ylab = ""
	)
	plot.poly(tau, y)
	dev.off()
}

# sigma, non-informative prior
s <- rev(sqrt(1 / tau))
g <- 1.0E-2
y <- dgamma(tau, g, g)
dev.on("gammaS", width = width, height = height)
	par(mar = c(2.5, 1.5, 0.5, 0.5))
plot(
	s, y, type = "n",
	ylim = ylim,
	xlab = "s",
	ylab = ""
)
plot.poly(s, y)
dev.off()


