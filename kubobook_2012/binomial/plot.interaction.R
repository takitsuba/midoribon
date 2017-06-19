# R --vanilla!
source("glm.R")

dev.on("plot_interaction", width = width * 1.5, height = height * 1.05 / 1.5, top = 0.1)
par(mfrow = c(1, 2))
plot.xf <- function(fit, main = NA) {
	plot(d$x, d$y, type = "n", main = main, xlab = "", ylab = "")
	xx <- seq(min(d$x), max(d$x), length = 50)
	ff <- factor("C", levels = c("C", "T"))
	q <- predict(fit, newdata = data.frame(x = xx, f = ff), type = "response")
	lines(xx, q * 8, lwd = 3)
	ff <- factor("T", levels = c("C", "T"))
	q <- predict(fit, newdata = data.frame(x = xx, f = ff), type = "response")
	lines(xx, q * 8, col = "gray", lwd = 3)
}
plot.xf(fit.xf)#, main = "cbind(y, N - y) ~ x + f")
#legend("topleft", legend = c("C", "T"), lwd = 3, col = c("black", "gray"))
plot.xf(fit.xfi)#, main = "cbind(y, N - y) ~ x * f")
dev.off()

# dummy
dev.on("plot_int_dummy", width = width * 0.75, height = height * 0.75, top = 0.1)
fit.xfi$coefficients["fT"] <- 25
fit.xfi$coefficients["x:fT"] <- -2.5
plot.xf(fit.xfi, main = "")
#legend("topleft", legend = c("C", "T"), lwd = 3, col = c("black", "gray"))
dev.off()

