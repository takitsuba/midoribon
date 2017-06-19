source("COMMON.R")
source("generate.steps.R")

dev.on("stationary", width = 5.0, height = 2.5)

step.max <- 500
x <- 0:step.max
dd <- delta.l * 0.5
# trace
layout(matrix(1:2, 1, 2), width = c(10, 1))
par(mar = c(2.5, 2.5, 0.0, 0.0), mgp = c(1.5, 0.5, 0))
plot(
	x, numeric(length(x)),
	type = "n",
	xlim = c(-1, max(x) + 1),
	ylim = range(vq) + c(-1, 1) * dd,
	xlab = "",
	ylab = "",
	xaxs = "i" # internal
)
# taces
get.df.mcmc <- function(i)
{
	n.thin <- 1
	cat("# sampling under step.max =", step.max, " n.thin =", n.thin, "\n")
	sample <- generate.steps(i, step.max = step.max, n.thin = n.thin)
	data.frame(step = seq(1, step.max * n.thin, n.thin), sample = sample)
}

for (i.start in c(1, 8, 15, 22, 29, 36, 41)) {
	d <- get.df.mcmc(i = i.start)
	lines(
		d$step, vq[d$sample],
		lwd = 1,
		col = "#00000080"
	)
}

par(mar = c(2.5, 0.2, 0.0, 0.0), mgp = c(1.5, 0.5, 0))
plot(
	density.logL, vq, type = "l",
	axes = FALSE,
	xlab = "", ylab = "",
	lwd = 2
)

# end

dev.off()

