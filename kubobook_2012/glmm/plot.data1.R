# R --vanilla!
source("COMMON.R")
source("f.R")
width <- 3
height <- 3

v.x <- 0:8
xx <- seq(1, 7, 0.1)

par.glmm <- function() par(mar = c(1.5, 1.5, 0.1, 0.1), mgp = c(1.5, 0.5, 0), cex = 1.0)
line.true <- function() lines(xx, logistic(-4 + 1 * xx) * max(v.x), lwd = 1, lty = 2)

d <- read.csv("data6a.csv")
# No RE, GLM
dev.on("plot0", width = width, height = height)
par.glmm()
plot.d(d, col = "black")
line.true()
dev.off()
# No RE, GLM
dev.on("plot1", width = width, height = height)
par.glmm()
plot.d(d, col = "black")
fitA <- glm(cbind(y, N - y) ~ x, data = d, family = binomial)
beta <- fitA$coefficients
line.true()
lines(xx, logistic(beta[1] + beta[2] * xx) * max(v.x), lwd = 3)
dev.off()
# d4
dev.on("plot1d4", width = width, height = width)
par.glmm()
d4 <- d[d$x == 4,]
plot(
	v.x, dbinom(v.x, 8, prob = mean(d4$y) / 8) * nrow(d4),
	type = "b",
	ylim = c(0, 6),
	pch = 20,
	xlab = "", ylab = ""
)
points(v.x, summary(factor(d4$y, levels = v.x)), pch = 1)
dev.off()

