source("COMMON.R")
dz <- 0.8 # improvement in log-likelihood
x <- c(1, 1, 2, 2, 3, 3)
y <- c(0.5, 1.5, 0, 2, dz, 2 + dz) + 0.5
lty <- "13"
lcol <- "#000000"

figbias <- function(xlim = c(0, 3), ylim = c(0.3, 2.7))
{
	plot(
		numeric(0), numeric(0), type = "n",
		xlim = xlim, ylim = ylim,
		xlab = "", ylab = "",
		yaxs = "i", axes = FALSE
	)
}
llpoints <- function(x, y, x1, y1, x2, y2) 
{
	points(
		x, y,
		pch = c(19, 21),
		bg = c("#000000", "#ffffff"),
		col = c("#808080", "#000000"),
		cex = 2
	)
	dy <- 0.12
	arrows(
		x1, y1 - dy,
		x2, y2 + dy,
		length = 0.10,
		angle = 30,
		lwd = 2,
		lend = "round",
		col = "#808080"
	)
}
linesarrows <- function()
{
	#lines(c(0.5, 1.5), c(1.5, 1.5), lty = lty, col = lcol)
	lines(c(1.5, 2.5), c(0.5, 0.5), lty = lty, col = lcol)
	lines(c(1.5, 2.5), c(2.5, 2.5), lty = lty, col = lcol)
	lines(c(0.5, 2.5), c(1.0, 1.0), lty = lty, col = lcol)
	lines(c(0.5, 2.5), c(2.0, 2.0), lty = lty, col = lcol)
	text(1.22, 1.25, 0.5)
	text(1.22, 1.75, 0.5)
	text(2.22, 0.75, 0.5)
	text(2.22, 2.25, 0.5)
	dy <- 0.05
	arrows(
		x[c(1, 3)] + 0.45, c(2, 2.5, 1.5, 1) - dy,
		x[c(1, 3)] + 0.45, c(1.5, 2, 1, 0.5) + dy,
		code = 3,
		length = 0.05,
		angle = 30,
		lwd = 0.7,
		lend = "round",
		col = "#000000"
	)
}

dev.on("bias", width = 3, height = 3)
par(mar = c(1.5, 0.1, 0.1, 0.1), mgp = c(1.5, 0.5, 0))
figbias()
linesarrows()
llpoints(
	x[1:4], y[1:4],
	x[c(1, 3)], y[c(2, 4)],
	x[c(1, 3)], y[c(1, 3)]
)
axis(1, at = 1:2, c("1", "2"))
lines(c(0.5, 2.5), c(0.3, 0.3))
dev.off()

dev.on("bias2", width = 3, height = 3)
par(mar = c(1.5, 0.1, 0.1, 0.1), mgp = c(1.5, 0.5, 0))
figbias(xlim = c(0, 4), ylim = c(0.3, 2.7 + dz))
axis(1, at = 1:3, c("1", "2", "2"))
lines(c(0.5, 3.5), c(0.3, 0.3))
lines(c(0.5, 3.5), c(1, 1), lty = lty, col = lcol)
lines(c(0.5, 3.5), c(2, 2), lty = lty, col = lcol)
llpoints(
	x, y,
	x[c(1, 3, 5)], y[c(2, 4, 6)],
	x[c(1, 3, 5)], y[c(1, 3, 5)]
)
dev.off()

