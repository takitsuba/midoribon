source("COMMON.R")
source("Rhat.R")
N <- 100
M <- 3
x <- 1:N
y <- matrix(rnorm(N * M), N, M)

plot.chains <- function(file, dy = rep(0, M), xticks = FALSE, add.caption = FALSE)
{
	my <- y + matrix(dy, N, M, byrow = TRUE)
	dev.on(file, width = 6.0, height = 3.0)
	ylim <- c(-5.5, 5.5)
	par(mar = c(2.0, 0.8, 0.1, 0.5), mgp = c(1.5, 0.5, 0))
	plot(
		x, numeric(length(x)),
		type = "n",
		xlim = c(-1, max(x) + 1),
		ylim = ylim,
		xlab = "",
		ylab = "",
		axes = FALSE,
		xaxs = "i" # internal
	)
	at <- pretty(x)
	ifelse(xticks, axis(1, at = at), axis(1, at = at, labels = rep("", length(at))))
	at <- pretty(ylim)
	axis(2, at = at, labels = rep("", length(at)))
	box()
	v.col <- c("#00000020", "#000000", "#00000020")
	v.lwd <- c(2, 1, 2)
	v.lty <- c(1, 2, 1)
	v.pch <- c(24, 16, 21)
	for (j in 1:M) {
		lines(
			x, my[,j],
			lwd = v.lwd[j],
			lty = v.lty[j],
			col = v.col[j]
		)
		points(
			x, my[,j],
			pch = v.pch[j],
			col = "#000000",
			bg = "#ffffff",
			cex = 0.6
		)
	}
	if (add.caption) for (j in 1:M) {
		xmax <- max(x)
		ymax <- max(ylim)
		yy <- ymax * (0.57 + 0.14 * j)
		lines(
			c(xmax * 0.9, xmax * 0.98), c(yy, yy),
			lwd = v.lwd[j], lty = v.lty[j], col = v.col[j]
		)
		points(
			xmax * 0.94, yy, pch = v.pch[j],
			col = "#000000", bg = "#ffffff", cex = 1.0
		)
		text(xmax * 0.82, yy, labels = sprintf("chain %i", j))
	}
	dev.off()
	rhat <- get.Rhat(my)
	cat(sprintf("%s: Rhat = %.3f, varp = %.3f\n", file, rhat[1], rhat[2]))
}

sink("Rhat.txt")
plot.chains("Rhat1", dy = c(-0.1, 0, 0.1), add.caption = TRUE)
plot.chains("Rhat2", dy = c(-1.2, 0, 1.2))
plot.chains("Rhat3", dy = c(-2.2, 0, 2.2), xticks = TRUE)
sink()

