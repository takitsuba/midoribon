source("COMMON.R")
source("set.data.R")
load("post.bugs.RData")
post.mcmc <- to.mcmc(post.bugs)
v <- 1:N.site
y.max <- 27 

plot.frame <- function(j)
{
	plot(
		Y[,1],
		type = "n",
		xlab = ifelse(j == 3, "location", ""),
		ylab = "abundance",
		ylim = c(0, y.max),
		axes = FALSE
	)
	at.x <- seq(0, 50, 10)
	labels <- rep("", length(at.x)) 
	if (j == 3) labels <- at.x
	axis(1, at = at.x, labels = labels)
	axis(2)
	box()
}


plot.re <- function(j = 1, n = 3, col1)
{
	n.sample <- nrow(post.mcmc)
	b <- Beta # fixed !!!
	for (k in 1:n) {
		i <- sample(n.sample, 1)
		re <- post.mcmc[
			i,
			sapply(1:N.site, function(k) sprintf("re[%i,%i]", j, k))
		]
		lines(
			exp(b + re),
			col = col1,
			lwd = 4
		)
	}
}

plot.pdf <- function(j, n, col1)
{
	file <- sprintf("tau%i", j)
	dev.on(
		file,
		#bg = "white",
		width = inch(28),
		height = inch(20) 
	)
	par(mar = c(2.8, 2.8, 0.1, 0.1), mgp = c(1.6, 0.5, 0), cex = 1.5)
	plot.frame(j)
	plot.re(j, n, col1)
	points(Y[,1])
	dev.off()
}

for (j in 1:3)
	plot.pdf(j, n = 3, col1 = "#00000040")

