# R --vanilla!
source("COMMON.R")

N <- length(data)
mean.y <- mean(data)
mean.true <- 3.5
y <- 0:9
ymax <- 11

plot.frame <- function(file, ymax = 14, width = 2.0, height = 2.0, xlab = "y")
{
	dev.on(file, width = width, height = height)
	par(mar = c(2.4, 1.5, 0.1, 0.1), mgp = c(1.3, 0.6, 0), cex = 1.2)
	plot(
		c(), c(),
		type = "n",
		xlab = xlab,
		ylab = "",
		yaxs = "i",
		axes = FALSE,
		xlim = range(y),
		ylim = c(0, ymax),
	)
	axis(1)
	box()
}


add.hist <- function(vy)
{
	hs <-hist(vy, breaks = seq(-0.5, 12.5, 1), plot = FALSE)
	lines(hs)
}

# histgram
plot.frame("randestH0")
add.hist(data)
dev.off()

# dpois
prob <- dpois(y, lambda = mean.true) # !!!
plot.frame("randestD0")
lines(y, prob * N)
points(y, prob * N, col = "#808080", pch = 19)
dev.off()

prob <- dpois(y, lambda = mean.y) # !!!
plot.frame("randestE0")
lines(y, prob * N, type = "b", lty = 3)
dev.off()

# new data
for (i in 1:3) {
	plot.frame(sprintf("randestH%i", i), xlab = "")
	add.hist(rpois(N, mean.true))
	lines(y, prob * N, type = "b", lty = 3)
	dev.off()
}

