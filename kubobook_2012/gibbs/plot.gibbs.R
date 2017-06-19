source("COMMON.R")

d.b1b2 <- read.csv("change/b1b2.csv")
for (i in 1:nrow(d.b1b2)) {
	for (j in 1:2) {
		file <- sprintf("change/step%ib%i.tex", i - 1, j)
		cat("# generating", file, "...\n")
		sink(file)
		cat(sprintf("$\\beta_%i = %.3f$", j, d.b1b2[i, j]))
		sink()
	}
}

v.b1 <- seq(1.4, 2.6, 0.002)
v.b2 <- seq(-0.1, 0.1, 0.0005)
v.range <- c(0.95, 0.75, 0.55, 0.35, 0.15)

get.prob <- function(bb1, bb2)
{
	exp(sum(dpois(d$y, exp(bb1 + bb2 * d$y), log =TRUE)))
}
d2p <- function(vd)
{
	cs <- cumsum(vd)
	list(d = vd, p = cs / max(cs))
}
get.prob.b1 <- function(vb1, bb2)
{
	d2p(sapply(vb1, function(bb1) get.prob(bb1, bb2)))
}
get.prob.b2 <- function(bb1, vb2)
{
	d2p(sapply(vb2, function(bb2) get.prob(bb1, bb2)))
}

draw.envelope <- function(b1L, b2L, b1U, b2U)
{
	rx <- rev(d$x)
	polygon(
		c(d$x, rx),
		c(exp(b1L + b2L * d$x), exp(b1U + b2U * rx)),
		border = NA,
		col = "#00000010"
	)
}
draw.envelope.b1 <-function(vb1, b2)
{
	draw.envelope(vb1[1], b2, vb1[2], b2)
}
draw.envelope.b2 <-function(b1, vb2)
{
	draw.envelope(b1, vb2[1], b1, vb2[2])

}
qb <- function(vb, pb, range)
{
	a <- (1 - range) * 0.5
	range(vb[pb >= a & pb <= (1 - a)])
}

plot.d <- function(step, j, v.b, pb, y.max, xlab, xlim)
{
	pd <- pb$d / sum(pb$d)
	dev.on(sprintf("step%ib%id", step, j), width = 2.1, height = 1.7)
	par(mar = c(2.5, 1.0, 0.0, 1.0), mgp = c(1.4, 0.4, 0))
	plot(
		v.b, pd,
		type = "l",
		xlim = xlim,
		ylim = c(0, y.max),
		yaxs = "i",
		xlab = xlab,
		ylab = "",
		axes = FALSE
	)
	axis(1)
	abline(h = 0)
	beta.next <- d.b1b2[step + 1, j]
	lines(c(beta.next, beta.next), c(0, y.max), lty = 2, col = "#808080")
	arrows(
		beta.next, y.max, beta.next, y.max * 0.7,
		lwd = 2, length = 0.2,
		lend = "round", ljoin = "round"
	)
	dev.off()
}

plot.b1b2 <- function(step, bb1, bb2)
{
	# b1 and data x, y
	pb1 <- get.prob.b1(v.b1, bb2)
	plot.frame(sprintf("step%ib1xy", step))
	for (i in 1:length(v.range)) draw.envelope.b1(qb(v.b1, pb1$p, v.range[i]), bb2)
	add.xy()
	dev.off()

	# b2 and data x, y
	pb2 <- get.prob.b2(bb1, v.b2)
	plot.frame(sprintf("step%ib2xy", step))
	for (i in 1:length(v.range)) draw.envelope.b2(bb1, qb(v.b2, pb2$p, v.range[i]))
	add.xy()
	dev.off()

	plot.d(step, 1, v.b1, pb1, 0.015, expression(beta[1]), c(1.6, 2.4))
	plot.d(step, 2, v.b2, pb2, 0.030, expression(beta[2]), c(-0.03, 0.06))
}

plot.change <- function(i)
{
	plot.frame(sprintf("step%ib1change", i - 1))
	add.mean(d.b1b2[i - 1, 1], d.b1b2[i - 1, 2], lty = 3, lwd = 1)
	add.mean(d.b1b2[i, 1], d.b1b2[i - 1, 2], lty = 1, lwd = 2)
	add.xy()
	dev.off()
	plot.frame(sprintf("step%ib2change", i - 1))
	add.mean(d.b1b2[i, 1], d.b1b2[i - 1, 2], lty = 3, lwd = 1)
	add.mean(d.b1b2[i, 1], d.b1b2[i, 2], lty = 1, lwd = 2)
	add.xy()
	dev.off()
}

for (i in 2:nrow(d.b1b2)) {
	plot.b1b2(i - 1, bb1 = d.b1b2[i, "b1"], bb2 = d.b1b2[i - 1, "b2"])
	plot.change(i)
}
