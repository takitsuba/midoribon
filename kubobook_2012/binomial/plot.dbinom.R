source("COMMON.R")
width  <- 3.5 # inch
height <- 2.8 # inch

y <- 0:8
dev.on("dbinom", width = width, height = height)
plot(
	y, dbinom(y, max(y), prob = 0.1),
	type = "n",
	xlab = "",
	ylab = ""
)
for (q in c(0.1, 0.3, 0.8)) {
	lines(
		y, dbinom(y, max(y), prob = q),
		type = "b",
		col = rgb(0, 0, 0, min(q + 0.3, 1)),
		lwd = 2,
		pch = 16
	)
}
#legend("topright", legend = c(0.1, 0.3, 0.6), pch = c(21, 23, 24), title = "q", cex = 0.7)
dev.off()


