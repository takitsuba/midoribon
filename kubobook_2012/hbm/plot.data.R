source("COMMON.R")

n <- nrow(d)
size <- 8
q <- sum(d$y) / (size * n)
plot.data <- function() plot(
	0:size, summary(as.factor(d$y)),
	ylim = range(c(0, dbinom(0:size, size, q) * n)),
	xlab = "",
	ylab = "",
	pch = 19
)

width  <- 2.8 # inch
height <- 2.8 # inch

# data
dev.on("dataBinom", width = width, height = height, top = 0.1) # height = width!
plot.data()
points(0:size, dbinom(0:size, size, q) * n, type = "b")
dev.off()

# GLMM
library(glmmML)
source("gaussianbinom.R")
fit <- glmmML(cbind(y, size - y) ~ 1, family = binomial, cluster = id, data = d)
beta <- fit$coefficients[1]
sigma <- fit$sigma

dev.on("dataGlmm", width = width, height = height, top = 0.1)
plot.data()
points(
	0:size,
	d.gaussian.binom(0:size, size, fixed = beta, sd = sigma) * n,
	type = "b"
)
dev.off()

# WinBUGS
source("R2WBwrapper.R")
load("post.bugs.RData")
post.mcmc <- to.mcmc(post.bugs)

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
	cat("# generating mp ...\n")
	mp <- sapply(
		1:nrow(post.mcmc),
		function(i) d.gaussian.binom(
			0:size, size,
			fixed = beta[i],
			sd = sigma[i]
		)
	)
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

dev.on("qdata1", width = width, height = height, top = 0.1)
plot.data()
#plot.polygon(0.95)
plot.lines(mp * n)
plot.median()
dev.off()

dev.on("qdata2", width = width, height = height, top = 0.1)
plot.data()
plot.lines(my)
plot.median()
dev.off()

dev.on("qdata3", width = width, height = height, top = 0.1)
plot.data()
q.env <- apply(my, 1, quantile, probs = c(0.025, 0.975))
polygon(
	c(0:size, size:0),
	c(q.env[1,], rev(q.env[2,])), # 0.025 and 0.975
	border = FALSE,
	col = "#00000020"
)
plot.median()
dev.off()
