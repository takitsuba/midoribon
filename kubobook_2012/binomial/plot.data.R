source("glm.R")

plot.data <- function(...)
{
	plot(
		d$x, d$y,
		pch = c(21, 16)[d$f],
		xlab = "", ylab = "",
		...
	)
}

# plot x, y
dev.on("plot_xy", width = width, height = height, top = 0.1)
plot.data()
legend("topleft", legend = c("C", "T"), pch = c(21, 19))
dev.off()

# plot fit
xx <- seq(min(d$x), max(d$x), length = 50)

dev.on("plot_fit_xfC", width = width, height = height, top = 0.1)
plot.data(col = c("#000000", NA)[d$f])
ff <- factor("C", levels = c("C", "T"))
p <- predict(fit.xf, newdata = data.frame(x = xx, f = ff), type = "response")
lines(xx, p * 8, lwd = 3, col = "#000000")
dev.off()

dev.on("plot_fit_xfT", width = width, height = height, top = 0.1)
plot.data(col = c(NA, "#000000")[d$f])
ff <- factor("T", levels = c("C", "T"))
p <- predict(fit.xf, newdata = data.frame(x = xx, f = ff), type = "response")
lines(xx, p * 8, lwd = 3, col = "#00000080")
dev.off()
