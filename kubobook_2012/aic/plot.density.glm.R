source("COMMON.R")
load("data.RData")
load("glm.RData")

d1 <- m.fit[2,] - m.fit[1,]
d2 <- m.mll[2,] - m.mll[1,]

v.brk <- seq(-0.5, 5, 0.25)
m.brk <- matrix(c(v.brk, -v.brk), length(v.brk), 2)
plot.hist <- function(key)
{
	dd <- d1
	if (key == 2) dd <- d2
	dev.on(sprintf("densityglm%i", key), width = 3, height = 1.5)
	par(mar = c(1.6, 0.2, 0.2, 0.2), mgp = c(1.5, 0.5, 0))
	h <- hist(dd, breaks = m.brk[,key], plot = FALSE)
	plot(
		h,
		ylim = c(0.04 * 110, 110),
		main = "", xlab = "", ylab = "",
		axes = FALSE,
		xaxs = "i", yaxs = "i"
	)
	abline(v = mean(dd), lty = 2)
	abline(h = 0)
	axis(1)
	dev.off()
}
plot.hist(1)
plot.hist(2)

dev.on("densityglm3", width = 3, height = 1.5)
par(mar = c(1.6, 0.2, 0.2, 0.2), mgp = c(1.5, 0.5, 0))
delta <- d1 - d2
xy <- density(delta)
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

sink("glm.txt")
cat("# summary(delta)\n")
print(summary(delta))
cat("\n")
cat("# summary(d1)\n")
print(summary(d1))
cat("\n")
cat("# summary(d2)\n")
print(summary(d2))
cat("\n")
sink()

