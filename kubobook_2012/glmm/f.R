logistic <- function(z) 1 / (1 + exp(-z))

d <- data.frame(
	N = 8,
	x = rep(2:6, each = 20),
	re = 0.0
)
d$y <- rbinom(nrow(d), d$N, prob = logistic(-4 + 1 * d$x + d$re))

plot.d <- function(
	d, w = 0.1, col = "#ff4000",
	xlab = "", ylab = "",
	axes = TRUE,
	ylim = range(d$y),
	...
) {
	plot(
		d$x, d$y, type = "n", # no plot
		xlab = xlab, ylab = ylab,
		axes = axes,
		ylim = ylim
	)
	for (x in sort(unique(d$x))) {
		dsub <- d[d$x == x,]
		for (ns in sort(unique(dsub$y))) {
			n <- sum(dsub$y == ns)
			sx <- 1:n
			sdx <- ifelse(n == 1, 1, sd(sx))
			points(
				x + (sx - mean(sx)) / sdx * w,
				rep(ns, n),
				col = col,
				...
			)
		}
	}
}
