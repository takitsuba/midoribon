source("COMMON.R")
dev.on("wald", width = 8, height = 3, top = 0)
par(mar = c(1.5, 0.5, 0.1, 0.5), cex = 1.5)
x <- seq(-0.2, 1.6, 0.01)
plot(
	x,
	dnorm(x, 1.292, 0.367),
	type = "l", axes = FALSE, xlab = "", ylab = "",
	xaxs = "i", yaxs = "i",
	ylim = c(0, dnorm(0, 0, 0.0356))
)
abline(h = 0)
abline(v = 0, lwd = 1)
axis(1)
x <- seq(-0.2, 0.2, length = 50)
dn <- function(z) dnorm(z, 0.0757, 0.0356)
lines(x, dn(x))
x <- seq(-0.2, 0, length = 20)
polygon(c(x, 0), c(dn(x), 0), border = NA, col = "#000000")
dev.off()
