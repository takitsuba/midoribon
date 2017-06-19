source("COMMON.R")
source("set.data.R")
load("post.bugs.RData")
post.mcmc <- to.mcmc(post.bugs)
v <- 1:N.site
y.max <- 27 

plot.frame <- function(v.no = 9999, ylab, sub = NA, col2)
{
	plot(
		Y[,1],
		type = "n",
		xlab = "location",
		ylab = ylab,
		ylim = c(0, y.max),
		axes = FALSE
	)
	axis(1)
	axis(2, labels = (ylab != ""))
	box()
	#if (!is.na(sub)) text(2, y.max - 2, sub, cex = 1.5)
	for (x in v.no) {
		polygon(
			c(x - 0.51, x + 0.51, x + 0.51, x - 0.51),
			c(0, 0, y.max + 2, y.max + 2),
			border = col2,
			col = col2,
		)
	}
}

plotY <- function(v.no = 9999)
{
	points(Y[,1], pch = 21 - 2 * (v %in% v.no))
	lines(m, lwd = 2, lty = 2)
}

plot.re <- function(j, col1, col2)
{
	mre <- sapply(
		v, function(i) quantile(
			post.mcmc[, sprintf("r[%i,%i]", j, i)],
			probs = c(0.5, 0.025, 0.975)
		)
	)
	b <- median(post.mcmc[,sprintf("beta[%i]", j)])
	polygon(
		c(v, rev(v)),
		exp(b + c(mre[2,], rev(mre[3,]))),
		border = NA,
		col = col2
	)
	lines(
		exp(b + mre[1,]),
		col = col1,
		lwd = 2
	)
}

plot.pdf <- function(j, col1 = "#000000", col2 = "#00000030", v.no,
	no.re = FALSE, ylab = "abundance", sub = NA
) {
	file <- sprintf("plot%i%s", j, ifelse(no.re, "data", ""))
	dev.on(
		file,
		width = inch(28),
		height = inch(20) 
	)
	par(mar = c(2.8, 2.8, 0.1, 0.1), mgp = c(1.6, 0.5, 0), cex = 1.5)
	plot.frame(v.no, ylab, sub, col2 = "#bbbbbb")
	if (!no.re) plot.re(j = j, col1, col2)
	plotY(v.no)
	dev.off()
}

plot.pdf(j = 1, v.no = 9999, sub = "(B)", ylab = "")
plot.pdf(j = 2, v.no = no, sub = "(B)", ylab = "")
plot.pdf(j = 3, v.no = 9999, sub = "(A)")
plot.pdf(j = 4, v.no = no, sub = "(A)")

plot.pdf(j = 1, v.no = 9999, no.re = TRUE)
plot.pdf(j = 2, v.no = no, no.re = TRUE)
