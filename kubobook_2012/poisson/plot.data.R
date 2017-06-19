# R --vanilla!
source("COMMON.R")

width  <- 4 # inch
height <- 3 # inch

# plot x, y
dev.on("plot_xy", width = width, height = height, top = 0.1)
plot(d$x, d$y, pch = c(21, 19)[d$f])
legend("topleft", legend = c("C", "T"), pch = c(21, 19))
dev.off()

# plot f, y
dev.on("plot_fy", width = width, height = height, top = 0.1)
plot(d$f, d$y)
dev.off()

# plot fit
dev.on("plot_fit", width = width, height = height, top = 0.1)
plot(d$x, d$y, pch = c(21, 19)[d$f])
xx <- seq(min(d$x), max(d$x), length = 50)
lines(xx, exp(1.29 + 0.0757 * xx), lwd = 2)
dev.off()

# plot exp
dev.on("plot_exp", width = width, height = height, top = 0.1)
xi <- seq(-4, 5, 0.1)
plot(
	xi, exp(-1 + 0.4 * xi), type = "l",
	xlab = "", ylab = "",
	lwd = 2
)
lines(xi, exp(-2 - 0.8 * xi), lwd = 2, lty = 2)
abline(v = 0, lwd = 2, lty = 3)
dev.off()
