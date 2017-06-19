source("COMMON.R")

dev.on("likelihood", width = width, height = height, top = 1)
lambda <- seq(2, 5, 0.1)
plot(
	lambda,
	sapply(lambda, logL.pois),
	type = "l",
	xlab = "",
	ylab = "log likelihood"
)
abline(v = mean(data), lty = 2)
points(mean(data), logL.pois(mean(data)), pch = "*", cex = 3)
dev.off()
