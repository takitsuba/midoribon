# R --vanilla!
source("COMMON.R")

dev.on("plot_dame", width = width, height = height, top = 0.5)
d$warizan <- d$y / d$N
plot(d$x, d$warizan, pch = c(21, 19)[d$f], ylim = c(-0.05, 1.05), xlab = "", ylab = "")
dC <- d[d$f == "C",]
abline(lm(warizan ~ x, data = dC), lwd = 3)
dT <- d[d$f == "T",]
abline(lm(warizan ~ x, data = dT), lwd = 3, col = "gray")
abline(h = c(0, 1), lty = 3)
dev.off()
