source("COMMON.R")

width  <- 4 # inch
height <- 3.2 # inch

yy <- function(f = 1) max(vlogL) + (vlogL - max(vlogL)) * f
vf <- exp(seq(log(0.1), log(2.0), length = 8))
mat.y <- sapply(vf * 2, yy)
# likelihood curves
dev.on("logLcurves", width = width, height = height)
plot(
	vq, vlogL,
	type = "n",
	xlab = "",
	ylab = "log likelihood"
)
for (i in 1:length(vf)) {
	lines(vq, mat.y[,i], col = gray(1 - i / length(vf)), lwd = 2)
}
abline(v = q.hat, lty = 2)
points(q.hat, logL.binom(q.hat), pch = "*", cex = 3)
dev.off()

cex <- 1.2
# likelihood -> posterior
dev.on("post1", width = width, height = height)
plot.qdist <- function(likelihood = FALSE, func = function(){})
{
	par(cex = cex)
	plot(
		vq, density.logL,
		type = "l",
		lwd = 1,
		pch = 21,
		bg = "white",
		xlab = "",
		ylab = "",
		ylim = c(0, max(density.logL) * 1.1),
		axes = FALSE
	)
	func()
	points(vq, density.logL, pch = 21, bg = "#ffffff")
	axis(1)
	vp = c(0, 0.05, 0.1)
	if (likelihood) {
		axis(2, at = vp, label = round(vp * sum(exp(vlogL)) * 1.0e+17, 2))
	} else {
		axis(2, at = vp)
	}
	box()
}
plot.qdist()
dev.off()
dev.on("post1noyaxs", width = width, height = height)
plot.qdist(likelihood = TRUE)
dev.off()

# likelihood -> posterior
# with samples
qhist <- function()
{
	s <- output.mcmc3$sample
	dd <- delta.l * 0.5
	l.hist <- hist(
		vq[s],
		breaks = seq(min(vq) - dd, max(vq) + dd, delta.l),
		plot = FALSE
	)
	lcount <- l.hist$counts
	lfreq <- lcount / sum(lcount)
	rect(
		vq - delta.l * 0.30,
		rep(0, length(vq)),
		vq + delta.l * 0.30,
		rep(lfreq, length(vq)),
		col = "#808080",
		border = NA
	)
}
load("output.mcmc.RData")
dev.on("post2", width = width, height = height)
plot.qdist(func = function() qhist())
dev.off()


