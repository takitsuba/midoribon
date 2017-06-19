# R --vanilla!
source("COMMON.R")
source("gaussianbinom.R")

nml <- function(x) x / sum(x)

v.x <- seq(0.001, 0.999, length = 200)
ab <- 20
y.max <- dbeta(0.5, ab, ab) * 1.01
plot.prior <- function(title, v.y)
{
	file <- paste("priorexample", title, sep = "")
	dev.on(file, width = 2.5, height = 2.5)
	par(mar = c(2.0, 0.5, 0.1, 0.5), xpd = TRUE)
	plot(
		v.x, rep(0, length(v.x)),
		type = "n",
		lty = 2,
		xlab = "", ylab = "",
		ylim = c(0, y.max),
		yaxs = "i",
		axes = FALSE
	)
	if (!is.na(v.y[1])) polygon(
		c(min(v.x), v.x, max(v.x)),
		c(0, v.y, 0), # assuming mu = 0!
		border = NA,
		col = "#c0c0c0"
	)
	axis(1)
	abline(h = 0, lwd = 2)
	#text(0, y.max * 0.94, sprintf("(%s)", title), cex = 1.5, pos = 4)
}

# priors
plot.prior("A", dbeta(v.x, 6, 14))
dev.off()
plot.prior("B", dbeta(v.x, 1, 1))
dev.off()
plot.prior("C", NA)
hcol <- "#00000010"
polygon(v.x, dbeta(v.x, ab / 6, ab / 6), col = hcol, border = NA)
polygon(v.x, dbeta(v.x, ab / 3, ab / 3), col = hcol, border = NA)
polygon(v.x, dbeta(v.x, ab, ab), col = hcol, border = NA)
dev.off()

