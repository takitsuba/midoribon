# R --vanilla!
source("COMMON.R")
plot.pois <- function(lambda)
{
	hist(
		data,
		breaks = seq(-0.5, 9.5),
		main = "",
		ylim = c(0, 15),
		xlab = "",
		ylab = "",
		border = "#808080"
	)
	y <- 0:9
	N <- length(data)
	prob <- dpois(y, lambda)
	lines(y, prob * N, type = "b", lty = 2)
	text(6, 15, sprintf("lambda = %-6.1f", lambda))
	text(6, 13, sprintf("log L = %-6.1f", logL.pois(lambda)))
}

dev.on("logL", width = 7, height = 7)
par(mfrow = c(3, 3)) 
par(mar = c(2.2, 2.2, 1.0, 0.5), mgp = c(1.5, 0.5, 0), cex = 1.0)
v.lambda <- seq(2.0, 5.2, 0.4)
for (lambda in v.lambda) {
	plot.pois(lambda)
}
dev.off()

