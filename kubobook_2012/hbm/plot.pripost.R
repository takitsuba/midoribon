# R --vanilla!
source("COMMON.R")
source("gaussianbinom.R")

logistic <- function(x) 1 / (1 + exp(-x))
logit    <- function(p) log(p / (1 - p))
nml <- function(x) x / sum(x)

Size <- 8
dx <- 0.002
v.x <- logit(seq(dx, 1 - dx, dx))

dev.onwr <- function(file)
{
	#dev.on(file, width = 5, height = 2 * 3)
	cat("# output to", file, "...\n")
	pdf(sprintf("%s.pdf", file), width = 5, height = 6, paper = "special")
	par(mar = c(2.0, 0.0, 0.1, 0.0), xpd = TRUE)
}

plot.prior <- function(sd, y.max, title)
{
	plot(
		v.x, rep(0, length(v.x)),
		type = "n",
		lty = 2,
		xlab = "", ylab = "",
		xlim = c(-5.9, 5.9),
		ylim = c(0, y.max),
		xaxs = "i", yaxs = "i",
		axes = FALSE
	)
	rn <- range(pnorm(v.x, 0, sd))
	total <- max(rn) - min(rn)
	polygon(
		c(min(v.x), v.x, max(v.x)),
		c(0, nml(dnorm(v.x, 0, sd)) * total, 0), # assuming mu = 0!
		border = NA,
		col = "#c0c0c0"
	)
###	text(
###		v.x[1] - 0.8, y.max * 0.8, 
###		eval(parse(text = sprintf(
###			'expression("(%s)"*" "*italic(s)*" = %g")', title, sd
###		))),
###		cex = 2, pos = 4
###	)
	axis(1)
	abline(h = 0, lwd = 2)
}

add.likelihood <- function(Y)
{
	v.y <- nml(dbinom(Y, size = Size, prob = logistic(v.x)))
	lines(v.x, v.y, lty = 2) # assuming mu = 0!
}
add.post <- function(Y, sd)
{
	v.y <- nml(f.gaussian.binom(
		alpha = v.x,
		x = Y,
		size = Size,
		mu = 0,
		sd = sd
	))
	lines(v.x, v.y, lwd = 2) # assuming mu = 0!
}

v.sd <- c(0.5, 1, 3)
v.Y <- c(2, 3, 5)
y.max <- max(nml(dnorm(v.x, 0, v.sd[1])))

# post ~ likelihood * prior
dev.onwr("pripost1")
par(mfrow = c(length(v.sd), 1))
for (i in 1:length(v.sd)) {
	sd <- v.sd[i]
	plot.prior(sd, y.max * 1.3, LETTERS[i])
	add.likelihood(v.Y[1])
	add.post(v.Y[1], sd)
}
dev.off()

# posteriors
dev.onwr("pripost2")
par(mfrow = c(length(v.sd), 1))
for (i in 1:length(v.sd)) {
	sd <- v.sd[i]
	plot.prior(sd, y.max * 1.3, LETTERS[i])
	abline(h = c(0.25, 0.5, 0.75, 1) * y.max, col = "#b0b0b0", lty = 3)
	for (y in v.Y) add.post(y, sd)
}
dev.off()

