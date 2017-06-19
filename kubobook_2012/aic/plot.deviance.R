source("COMMON.R")

dev.on("deviance", width = 3.5, height = 3.5)
par(mar = c(0.1, 0.1, 0.1, 0.1), mgp = c(1.5, 0.5, 0))
dmin <- 385.8
dmax <- 475.3
d1 <- 470.8
xmax <- 2
dd <- 5
plot(
	c(-2, xmax + 2), c(dmin - dd, dmax + dd),
	type = "n",
	ylim = c(dmin - dd, dmax + dd),
	xaxs = "i", yaxs = "i",
	xlab = "", ylab = "",
	axes = FALSE
)
lines(c(0, 0), c(dmin - 5, dmax + 5), lwd = 2)
lty <- "13"
lcol <- "#000000"
lines(c(-0.1, xmax), c(dmax, dmax), lty = lty, col = lcol)
lines(c(-0.1, xmax), c(dmin, dmin), lty = lty, col = lcol)
lines(c(-0.1, 0.3), c(d1, d1), lty = lty, col = lcol)
lines(c(0.8, xmax - 0.3), c(d1, d1), lty = lty, col = lcol)
text(-0.7, dmin, dmin)
text(-0.7, dmax, dmax)
text(-0.7, d1, d1)
text(0.7, 430, "89.5 (Null Deviance)", srt = 270)
text(1.7, 424.5, "85.0 (Residual Deviance)", srt = 270)
dy <- 0.7
arrows(
	c(0.5, 1.5), c(dmin, dmin) + dy,
	c(0.5, 1.5), c(dmax, d1) - dy,
	length = 0.10,
	angle = 30,
	code = 3,
	lwd = 2,
	lend = "round",
	col = "#808080"
)
dev.off()
