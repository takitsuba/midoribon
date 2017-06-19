load("d.RData")

fitP.f <- glm(y ~ f, family = poisson, data = d.long)
fitP.null <- glm(y ~ 1, family = poisson, data = d.long)

fitG.f <- glm(y.mean ~ f, family = gaussian(link = "log"), data = d)
fitG.null <- glm(y.mean ~ 1, family = gaussian(link = "log"), data = d)

sink("d.txt")
print(d)
print(d.long[, c("f", "block", "y")])
sink()

sink("glm.txt")
print(summary(fitP.f))
print(summary(fitG.f))
#print(fitP.null)
#print(fitG.null)
sink()

source("../COMMON.R")

width  <- 5 # inch
height <- 3.5 # inch

dev.on("plot1", width = width, height = height, top = 1)
par(cex = 1.3)
plot(d.long$f, d.long$y, ylim = c(0, 11)) # box-whisker
points(jitter(as.numeric(d.long$f), 0.2), d.long$y, pch = 4)
dev.off()

dev.on("plot2", width = width, height = height, top = 1)
par(cex = 1.3)
plot(d$f, d$y.mean, ylim = c(0, 11)) # box-whisker
points(as.numeric(d$f), d$y.mean, pch = 4)
dev.off()
