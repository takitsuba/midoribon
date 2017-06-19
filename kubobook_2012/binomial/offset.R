# R --vanilla!
source("COMMON.R")
d <- read.csv("data4b.csv")

#v.cex <- d$A * 0.2
v.cex <- 1.2
col.a <- function(x, o = 0.1) rgb(0, 0, 0, (1 - x) * (1 - 2 * o) + o)
v.col <- col.a(d$x)

plot.offset <- function(file, xx)
{
	dev.on(file, width = width, height = height, top = 0.4)
	plot(
		xx, d$y,
		xlab = "", ylab = "",
		cex = v.cex, col = v.col, pch = 1,
		xlim = c(0, max(xx) * 1.05),
		ylim = c(0, max(d$y) * 1.05)
	)
}

plot.offset("offsetX", d$x)
dev.off()
plot.offset("offsetA", d$A)
dev.off()

fit <- glm(y ~ x, offset = log(A), data = d, family = poisson)
b <- fit$coefficients
plot.offset("offsetA2", d$A)
v.a <- c(0, 20)
for (x in c(0.1, 0.3, 0.5, 0.7, 0.9)) {
	lines(v.a, v.a * exp(b[1] + b[2] * x), col = col.a(x, o = 0), lwd = 3)
}
dev.off()

