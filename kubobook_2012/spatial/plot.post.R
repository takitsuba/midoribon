source("COMMON.R")
source("set.data.R")
load("post.bugs.RData")
post.mcmc <- to.mcmc(post.bugs)

plot.post <- function(key, xlim, xpr)
{
	file <- sprintf("post%s", key)
	dev.on(file, width = 4, height = 4)
	par(mar = c(1.5, 0.5, 0.1, 0.5), mgp = c(1.5, 0.5, 0), cex = 1.5)

	xy <- density(post.mcmc[,sprintf("%s[3]", key)])
	x <- xy$x
	y <- xy$y / (sum(xy$y) * xy$bw)
	plot(
		x, y,
		xlim = xlim,
		type = "n", axes = FALSE,
		xlab = "", # xlab = key,
		ylab = "",
		xaxs = "i", yaxs = "i"
	)
	axis(1)
	#abline(h = 0, v = 0)
	if (key == "beta") {
		ypr <- dnorm(xpr, 0.0, 10^2)
	} else {
		ypr <- dgamma(xpr, 10^{-4}, 10^{-4})
	}
###	polygon( # prior
###		c(xpr[1], xpr, xpr[length(xpr)]),
###		c(0, ypr, 0),
###		border = NA,
###		col = "#808080"
###	)
	lines(x, y, lwd = 2)
	dev.off()
}

plot.post("beta", xlim = c(-0.1, 3), xpr = seq(-1, 4, lenght = 100))
plot.post("s", xlim = c(-0.5, exp(4.5)), xpr = exp(seq(-4, 4.5, length = 100)))


