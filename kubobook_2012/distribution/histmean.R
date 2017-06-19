source("COMMON.R")

mean <- sapply(1:3000, function(i) mean(rpois(50, 3.5)))

dev.on("histmean", width = 2.8, height = 2.1, top = 0.1)
par(mar = c(1.5, 1.5, 0.1, 0.1), mgp = c(1.5, 0.5, 0))
hist(
	mean, seq(2.4, 4.6, 0.1),
	main = "",
	xlab = "",
	ylab = ""
)
dev.off()

