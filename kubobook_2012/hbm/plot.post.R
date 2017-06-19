source("./COMMON.R")

d <- read.csv("data7a.csv")
source("R2WBwrapper.R")
load("post.bugs.RData")
post.mcmc <- to.mcmc(post.bugs)

# plot(post.bugs)
dev.on("plotpost", width = 6, height = 6)
plot(post.bugs)
dev.off()

# parameters
plot.post <- function(
	key, xlim1, xlim2,
	y.max = NA,
	width = 3, height = 3,
	addprior = FALSE
) {
	k <- gsub("(\\[)(.*)(\\])", "\\2", key) # r[1] -> r1
	file <- sprintf("post%s", k)
	dev.on(file, width = width, height = height)
	par(mar = c(2.5, 0.5, 0.5, 0.5), mgp = c(1.3, 0.5, 0))
	s <- as.vector(post.mcmc[, key])
	xy <- density(s)
	if (is.na(y.max)) {
		y.max <- max(xy$y)
	}
	plot(
		xy$x, xy$y,
		type = "n",
		xlim = c(xlim1, xlim2),
		ylim = c(0, y.max),
		xlab = "",
		ylab = "",
		axes = FALSE,
		yaxs = "i"
	)
	if (addprior) polygon(prior.x, prior.y, col = "#c0c0c0", border = NA)
	axis(1)
	abline(h = 0)
	lines(xy$x, xy$y, lwd = 2)
}
xrange <- 10
y.max <- max(density(post.mcmc[,"beta"])$y)

plot.post("beta", -xrange, xrange, y.max)
dev.off()

prior.x <- seq(-xrange, xrange, length = 100)
prior.y <- dnorm(prior.x, 0, median(post.mcmc[, "s"]))

plot.post("r[1]", -xrange, xrange, y.max, addprior = TRUE)
dev.off()
plot.post("r[2]", -xrange, xrange, y.max, addprior = TRUE)
dev.off()
plot.post("r[3]", -xrange, xrange, y.max, addprior = TRUE)
dev.off()
plot.post("s", -1, 7)
#abline(v = 0, lty = 3)
dev.off()

# confidence interval, beta
beta <- as.vector(post.mcmc[, "beta"])
beta80 <- quantile(beta, probs = c(0.1, 0.9))

xy <- density(beta)
dev.on("betaCI", width = 3, height = 3)
par(mar = c(2.5, 0.5, 0.5, 0.5), mgp = c(1.3, 0.5, 0))
plot(
	xy$x, xy$y,
	type = "n",
	xlab = "beta",
	ylab = "",
	axes = FALSE,
	yaxs = "i"
)
selector <- xy$x >= beta80[1] & xy$x <= beta80[2]
polygon(
	c(beta80[1], xy$x[selector], beta80[2]),
	c(0, xy$y[selector], 0),
	col = "#c0c0c0",
	border = NA
)
axis(1)
abline(h = 0)
abline(v = beta80, lty = 2)
lines(xy$x, xy$y, lwd = 2)
dev.off()

