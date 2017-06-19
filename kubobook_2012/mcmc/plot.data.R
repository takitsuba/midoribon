source("COMMON.R")

width  <- 4 # inch
height <- 4 # inch

dev.on("histdata", width = width, height = height)
par(cex = 1.5)
hist(
	data,
	breaks = seq(-0.5, 8.5, 1),
	#freq = FALSE,
	xlab = "",
	ylab = "",
	main = ""
)
x <- 0:size
lines(
	x,
	length(data) * dbinom(x, size, prob = q.true),
	type = "b",
	lty = 2
)
dev.off()
