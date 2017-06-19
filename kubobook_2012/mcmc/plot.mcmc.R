source("COMMON.R")
load("output.mcmc.RData")

plot.dh <- function(file, data, plot.density = TRUE)
{
	dev.on(file, width = 5.0, height = 2.5)
	x <- data$step
	s <- data$sample
	dd <- delta.l * 0.5
	# trace
	layout(matrix(1:2, 1, 2), width = c(3, 1))
	par(mar = c(2.5, 2.5, 0.5, 0.2), mgp = c(1.5, 0.5, 0))
	plot(
		x,
		vq[s],
		type = "l",
		xlim = c(-1, max(x) + 1),
		ylim = range(vq) + c(-1, 1) * dd,
		xlab = "",
		ylab = "",
		col = "#808080",
		xaxs = "i" # internal
	)
	# histogram
	par(mar = c(2.5, 0.2, 0.5, 0.2), mgp = c(1.5, 0.5, 0))
	l.hist <- hist(
		vq[s],
		breaks = seq(min(vq) - dd, max(vq) + dd, delta.l),
		plot = FALSE
	)
	if (plot.density) {
		lcount <- l.hist$counts
		lfreq <- lcount / sum(lcount) / delta.l
		plot(
			lfreq, vq,
			type = "n", axes = FALSE, xlab = "", ylab = "",
			xlim = c(0, max(density.logL) * 1.5)
		)
		rect(
			rep(0, length(vq)),
			vq - delta.l * 0.30,
			#rep(lfreq, length(vq)),
			lfreq * 0.009,
			vq + delta.l * 0.30,
			col = "#808080",
			border = NA
		)
		lines(density.logL, vq, lwd = 2)
	} else {
		lines(x, vq[output.optim2$sample], lty = 3)
	}
	dev.off()
}

plot.dh("optim", output.optim1, plot.density = FALSE)
plot.dh("mcmc1", output.mcmc1)
plot.dh("mcmc2", output.mcmc2)
plot.dh("mcmc3", output.mcmc3)

