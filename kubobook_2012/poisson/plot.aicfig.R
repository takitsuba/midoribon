# R --vanilla!
source("COMMON.R")

width  <- 3.0 # inch
height <- 3.0 # inch
lwd <- 3

nxx <- 50
xx <- seq(min(d$x), max(d$x), length = nxx)
dC <- data.frame(x = xx, f = factor(rep("C", nxx)))
dT <- data.frame(x = xx, f = factor(rep("T", nxx)))
yy3 <- predict(fit, newdata = dC, type = "response")
yy4x <- predict(fit.xf, newdata = dC, type = "response")
yy4f <- predict(fit.xf, newdata = dT, type = "response")

plot.xy <- function(i)
{
	dev.on(sprintf("fitmodel%i", i), width = width, height = height)
	par(mar = c(1.5, 1.5, 0.1, 0.1), mgp = c(1.5, 0.5, 0))
	plot(
		d$x, d$y,
		type = "n",
		#pch = c(21, 19)[d$f],
		ylim = c(6, 10),
		xlab = "", ylab = ""
	)
}

# model null
plot.xy(1)
lines(xx, rep(mean(d$y), nxx), lwd = lwd)
dev.off()

# model f
plot.xy(2)
lines(xx, rep(mean(d[d$f == "C", "y"]), nxx), lwd = lwd, lty = 2)
lines(xx, rep(mean(d[d$f == "T", "y"]), nxx), lwd = lwd, col = "#c0c0c0")
dev.off()

# model x
plot.xy(3)
lines(xx, yy3, lwd = lwd)
dev.off()

# model x + f
plot.xy(4)
lines(xx, yy4x, lwd = lwd, lty = 2)
lines(xx, yy4f, lwd = lwd, col = "#c0c0c0")
dev.off()

