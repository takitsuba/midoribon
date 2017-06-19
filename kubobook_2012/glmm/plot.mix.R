source("COMMON.R")
width <- 1.8
height <- 1.8

logistic <- function(r) 1 / (1 + exp(-r))
lambda <- function(r) exp(0.5 + r)

# d4
plot.mix.DP <- function(
	key, ymaxD,
	v.xD, v.yD,
	sd, r
) {
	# Data distribution
	dev.on(sprintf("mix%sD", key), width = width, height = height)
	par(mar = c(1.5, 0.0, 0.0, 0.2), mgp = c(1.5, 0.5, 0), cex = 1.0)
	plot(
		v.xD, v.yD,
		type = "l",
		ylim = c(0, ymaxD),
		axes = FALSE
	)
	points(v.xD, v.yD, pch = 21, bg = "#ffffff")
	axis(1)
	dev.off()
	# Parameter distribution
	dev.on(sprintf("mix%sP", key), width = width, height = width)
	par(mar = c(1.5, 0.1, 0.1, 0.1), mgp = c(1.5, 0.5, 0), cex = 1.0)
	xr <- 2.7 * seq(-sd, sd, length = 100)
	plot(
		xr, dnorm(xr, 0, sd),
		type = "l",
		lwd = 3,
		ylim = c(0, dnorm(0, 0, sd) * 1.05),
		col = "#aaaaaa",
		yaxs = "i",
		axes = FALSE
	)
	abline(h = 0)
	axis(1)
	lines(c(r, r), c(0, dnorm(r, 0, sd)), lty = 2)
	points(r, dnorm(r, 0, sd), pch = 21, cex = 2, bg = "#ffffff")
	dev.off()
}

plot.mix <- function(key, v.r, ymaxD, v.xD, m.yD, sd, v.yMix, func, pname)
{
	for (i in 1:length(v.r)) {
		plot.mix.DP(
			sprintf("%s%i", key, i), ymaxD,
			v.xD, m.yD[,i],
			sd, v.r[i]
		)
		sink(sprintf("mix%s/q%i.tex", key, i))
		cat(sprintf("\\rbox{r = %.2f}{%s = %.2f}", v.r[i], pname, func(v.r[i])))
		sink()
		sink(sprintf("mix%s/p%i.tex", key, i))
		cat(sprintf("$p(r) = %.2f$", dnorm(v.r[i], 0, sd)))
		sink()
	}
	dev.on(sprintf("mix%sM", key), width = width, height = width, bg = "#dddddd")
	par(mar = c(2.5, 0.1, 0.1, 0.1), mgp = c(1.5, 0.5, 0), cex = 1.0)
	plot(
		v.xD, v.yMix[1:length(v.xD)],
		type = "l",
		ylim = c(0, max(v.yMix) * 1.4),
		xlab = "", ylab = "",
		axes = FALSE
	)
	points(v.xD, v.yMix[1:length(v.xD)], pch = 21, bg = "#ffffff")
	axis(1)
	dev.off()
}

# Binomial
sd <- 3
v.r <- c(-2.2, -0.6, 1.0, 2.6)
v.xD <- 0:8
m.yD <- sapply(
	v.r,  function(r) dbinom(v.xD, max(v.xD), logistic(r))
)
v.rr <- qnorm(seq(0.001, 0.999, length = 200), 0, sd)
m.yMix <- sapply(v.rr, function(r) dbinom(v.xD, max(v.xD), logistic(r)))
v.yMix <- apply(m.yMix, 1, mean)
plot.mix(
	key = "Binom", v.r = v.r,
	ymaxD = max(m.yD),
	v.xD = v.xD,
	m.yD = m.yD,
	sd = sd,
	v.yMix = v.yMix,
	func = logistic,
	pname = "q"
)

# Poisson
sd <- 1 # !!!
v.r <- c(-1.1, -0.3, 0.5, 1.3)
v.xD <- 0:10
m.yD <- sapply(
	v.r,  function(r) dpois(v.xD, lambda(r))
)
v.rr <- qnorm(seq(0.001, 0.999, length = 200), 0, sd)
v.xx <- 0:round(max(exp(v.rr)))
m.yMix <- sapply(v.rr, function(r) dpois(v.xx, lambda(r)))
v.yMix <- apply(m.yMix, 1, mean)
plot.mix(
	key = "Pois", v.r = v.r,
	ymaxD = max(m.yD),
	v.xD = v.xD,
	m.yD = m.yD,
	sd = sd,
	v.yMix = v.yMix,
	func = lambda,
	pname = "\\lambda"
)
