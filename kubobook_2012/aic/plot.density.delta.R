source("COMMON.R")
load("data.RData")

delta <- -(v.mean.mle - sapply(
	1:n.rep, function(j) sum(dpois(m.data[,j], v.mle[j], log = TRUE))
))
xy <- density(delta)

dev.on("densitydelta1", width = 3, height = 1.5)
par(mar = c(1.6, 0.2, 0.2, 0.2), mgp = c(1.5, 0.5, 0))
plot(
	xy$x,
	xy$y,
	xlim = c(-10, 10), ylim = c(0, 0.7),
	type = "n", xlab = "", ylab = "",
	axes = FALSE,
	yaxs = "i"
)
polygon(xy$x, xy$y, col = "#bbbbbb", border = NA)
abline(v = mean(delta), lty = 2)
axis(1)
abline(h = 0)
dev.off()
print(summary(delta))

ll <- function(jj) sum(dpois(m.data[,jj], v.mle[jj], log = TRUE))

sink("delta.txt")
cat("# summary(delta1)\n")
print(summary(delta))
cat("\n")
cat(sprintf("# lambda1 = %.2f\n", mean(m.data[,j1])))
cat(sprintf("# mll1      = %.2f\n", ll(j1)))
cat(sprintf("# mean.mll1 = %.2f\n", v.mean.mle[j1]))

cat("\n")
cat(sprintf("# lambda2 = %.2f\n", mean(m.data[,j2])))
cat(sprintf("# mll2      = %.2f\n", ll(j2)))
cat(sprintf("# mean.mll2 = %.2f\n", v.mean.mle[j2]))
sink()

