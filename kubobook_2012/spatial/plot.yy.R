source("COMMON.R")
load("Y.RData")
y <- Y

plot.yy <- function(file, y1, y2, ylab = expression(y[j]), ...)
{
	dev.on(file, width = 4, height = 4, top = 0.1)
	plot(y1, y2, xlab = expression(y[i]), ylab = ylab, ...)
	dev.off()
}

plot.yy("yy1", y[1:49], y[2:50], ylab = expression(y[i+1]))
plot.yy("yy2", y[1:49], sample(y[2:50]), pch = 20, col = "#000000")
plot.yy("yy3", y[1:49], sample(y[2:50]), pch = 20, col = "#000000")
plot.yy("yy4", y[1:49], sample(y[2:50]), pch = 20, col = "#000000")

