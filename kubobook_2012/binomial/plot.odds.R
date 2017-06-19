# R --vanilla!
source("glm.R")

# odds
dev.on("plot_odds", width = width, height = height, top = 1)
yodds <- d$y / (8 - d$y)
plot(
	d$x, log(yodds), pch = c(21, 19)[d$f],
	ylim = c(-3, 3),
	xlab = "", ylab = ""
)
xx <- seq(min(d$x) - 1, max(d$x) + 1, length = 50)
ff <- factor("C", levels = c("C", "T"))
z <- predict(fit.xf, newdata = data.frame(x = xx, f = ff))
lines(xx, z, lwd = 3)
ff <- factor("T", levels = c("C", "T"))
z <- predict(fit.xf, newdata = data.frame(x = xx, f = ff))
lines(xx, z, col = "gray", lwd = 3)
legend("topleft", legend = c("C", "T"), pch = c(21, 19))
dev.off()


