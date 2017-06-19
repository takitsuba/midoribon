d <- read.csv("data7a.csv")
library(R2WinBUGS)
if (!exists("post.bugs")) load("post.bugs.RData")
#post.mcmc <- to.mcmc(post.bugs)
post.mcmc <- as.mcmc(post.bugs$sims.matrix)

q <- sum(d$y) / (8 * nrow(d))
logistic <- function(z) 1 / (1 + exp(-z))
n <- nrow(d)
size <- 8
q <- sum(d$y) / (size * n)

f.gaussian.binom <- function(alpha, x, size, fixed, sd)
	dbinom(x, size, logistic(fixed + alpha)) * dnorm(alpha, 0, sd)

d.gaussian.binom <- function(v.x, size, fixed, sd) sapply(
	v.x, function(x) integrate(
		f = f.gaussian.binom,
		lower = -sd * 10,
		upper = sd * 10,
		# for f.gaussian.binom
		x = x,
		size = size,
		fixed = fixed,
		sd = sd
	)$value
)

plot.data <- function() plot(
	0:size, summary(as.factor(d$y)),
	ylim = range(c(0, dbinom(0:size, size, q) * n)),
	xlab = "",
	ylab = "",
	pch = 19
)

plot.polygon <- function(mm, p)
{
	pp <- 1 - p
	qp <- apply(mm, 1, quantile, probs = c(0.5 * pp, 1 - 0.5 * pp))
	polygon(
		c(0:size, size:0),
		c(qp[1,], rev(qp[2,])),
		border = NA,
		col = "#00000020"
	)
}

plot.lines <- function(mm)
{
	apply(
		mm, 2,
		function(x) lines(0:size, x, col = "#00000001", lwd = 2)
	)
}

beta <- post.mcmc[, "beta"]
sigma <- post.mcmc[, "s"]
if (!exists("my")) {
	cat("# generating mp ")
	mp <- sapply(
		1:nrow(post.mcmc),
		function(i) {
			if (i %% 100 == 0) cat(".")
			d.gaussian.binom(
				0:size, size,
				fixed = beta[i],
				sd = sigma[i]
			)
		}
	)
	cat(" done\n")
	my <- apply(
		mp, 2, function(prob) summary(
			factor(
				sample(0:size, n, replace = TRUE, prob = prob),
				levels = 0:size
			)
		)
	)
}

plot.median <- function()
{
	lines(
		0:size, apply(mp * n, 1, median),
		type = "b",
		col = "black",
		bg = "white",
		pch = 21
	)
}

plot.data()
q.env <- apply(my, 1, quantile, probs = c(0.025, 0.975))
polygon(
	c(0:size, size:0),
	c(q.env[1,], rev(q.env[2,])), # 0.025 and 0.975
	border = FALSE,
	col = "#00000020"
)
plot.median()
