source("COMMON.R")
load("output.mcmc.RData")

short.arrows <- function(x0, y0, x1, y1, r = 0.8)
{
	rr <- (1 - r) * 0.5
	xx0 <- rev(x0 + (x1 - x0) * rr)
	xx1 <- rev(x1 - (x1 - x0) * rr)
	yy0 <- rev(y0 + (y1 - y0) * rr)
	yy1 <- rev(y1 - (y1 - y0) * rr)
	arrows(
		xx0, yy0, xx1, yy1, 
		length = 0.08,
		col = c("#666666", "#999999"),
		lwd = 3,
		lend = "round"
	)
}

width  <- 3.5 # inch
height <- 2.8 # inch
xlab <- ""
ylab <- "log likelihood"
q0 <- vq[i.start]

cex.points <- 2
cex.bullets <- 0.6

ylim <- c(min(vlogL), max(vlogL) + 1.2)

# logL0: continuous q
dev.on("logL0", width = width, height = height, right = 0.3, top = 0.6)
plot(vq, vlogL, xlab = xlab, ylab = ylab, pch = 21, cex = cex.bullets, ylim = ylim, type = "l")
abline(v = q.hat, lty = 2)
points(q.hat, logL.binom(q.hat), pch = "*", cex = cex.points)
dev.off()

# logL1: discrete q
dev.on("logL1", width = width, height = height, right = 0.3, top = 0.6)
plot(vq, vlogL, xlab = xlab, ylab = ylab, pch = 21, cex = cex.bullets, ylim = ylim)
dev.off()

# logL1mcmc: discrete q + mcmc
dev.on("logL1mcmc", width = width, height = height, right = 0.3, top = 0.6)
plot(vq, vlogL, type = "n", xlab = xlab, ylab = ylab, ylim = ylim)
s <- output.mcmc1[, "sample"]
x <- vq[s]
vn <- 1:length(s)
y <- vlogL[s] + (vn - mean(vn)) * 0.03
col.mcmc <- "#00000060"
lines(x, y, col = col.mcmc, lwd = 1.5)
points(vq, vlogL, pch = 21, cex = cex.bullets)
dev.off()

# logL2: around q0
plot.xylines <- function(x)
{
	y <- logL.binom(x)
	lines(c(x, x), c(-999, y), lty = 3) # vertical
	lines(c(0, x), c(y, y), lty = 2, col = "#808080") # horizontal
	text(x - delta.l * 0.4, y, sprintf("%.2f", y), pos = 3, col = "#808080")
}
# for logL2 but to estimate ylim here ------------------
# ------------------------------------------------------
s2 <- (i.start - 2):(i.start + 2)
dev.on("logL2", width = width, height = height, right = 0.3, top = 0.6)
plot(
	vq[s2], vlogL[s2], xlab = xlab, ylab = ylab,
	type = "n"
)
plot.xylines(q0)
plot.xylines(q0 - delta.l)
plot.xylines(q0 + delta.l)
#plot.xylines(q0 + delta.l * 2)
points(vq, vlogL, cex = cex.points, bg = "#ffffff", pch = 21)
points(q0, vlogL[i.start], cex = cex.points, bg = "#808080", pch = 21)
s3 <- (i.start - 2):(i.start + 1)
dy <- 0.0
short.arrows(vq[s3], vlogL[s3] + dy, vq[s3 + 1], vlogL[s3 + 1] + dy, r = 0.6)
dev.off()

# logL2mcmc: around q0
vn <- 1:10
s <- output.mcmc1[vn, "sample"]
x <- vq[s]
y <- vlogL[s] + (vn - mean(vn)) * 0.25
dev.on("logL2mcmc", width = width, height = height, right = 0.3, top = 0.6)
plot(
	x, y,
	xlab = xlab, ylab = ylab,
	type = "n",
	xlim = range(x),
	ylim = range(y) + c(-0.15, 0.05)
)
points(vq, vlogL, cex = cex.points, bg = "#ffffff", pch = 21)
points(q0, vlogL[i.start], cex = cex.points, bg = "#808080", pch = 21)
#points(x, y, pch = 19, cex = 1, col = col.mcmc)
v <- 1:(length(s) - 1)
short.arrows(x[v], y[v], x[v + 1], y[v + 1], r = 0.66)
#text(x - 0.0021, y + 0.036, 1:length(x), cex = 1, col = col.mcmc)
dev.off()

# logL3: optimization arrows
curved.arrows <- function(
	vi, delta.y = 1.0,
	col = col.mcmc,
	lwd = 3
) {
	vy <- vlogL + delta.y
	lines(vq[vi], vy[vi], lwd = lwd, col = col, lend = "round")
	n <- length(vi)
	arrows(
		vq[vi[n - 1]], vy[vi[n - 1]],
		vq[vi[n]], vy[vi[n]],
		col = col,
		lwd = lwd,
		lend = "round"
	)
}
dev.on("logL3", width = width, height = height, right = 0.3, top = 0.6)
plot(
	vq, vlogL, xlab = xlab, ylab = ylab, ylim = ylim
)
curved.arrows(i.start:21)
n <- length(vq)
curved.arrows((i.start + 30):23)
dev.off()

