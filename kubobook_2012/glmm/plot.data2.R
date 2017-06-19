# R --vanilla!
source("COMMON.R")
source("f.R")
width <- 3
height <- 3

v.x <- 0:8
xx <- seq(1, 7, 0.1)
par.glmm <- function() par(mar = c(1.5, 1.5, 0.1, 0.1), mgp = c(1.5, 0.5, 0), cex = 1.0)
line.true <- function() lines(xx, logistic(-4 + 1 * xx) * max(v.x), lwd = 1, lty = 2)

# RE
d <- read.csv("data6b.csv")
dev.on("plot00", width = width, height = height)
par.glmm()
plot.d(d, col = "black")
line.true()
dev.off()
dev.on("plot2", width = width, height = height)
par.glmm()
plot.d(d, col = "black")
fitB <- glm(cbind(y, N - y) ~ x, data = d, family = binomial)
beta <- fitB$coefficients
line.true()
lines(xx, logistic(beta[1] + beta[2] * xx) * max(v.x), lwd = 3)
dev.off()

# d4
dev.on("plot3", width = width, height = width)
par.glmm()
d4 <- d[d$x == 4,]
plot(
	v.x, dbinom(v.x, 8, prob = 0.506) * nrow(d4),
	type = "b",
	ylim = c(0, 6),
	pch = 20,
	xlab = "", ylab = ""
)
points(v.x, summary(as.factor(d4$y)), pch = 1)
dev.off()
# d4 plot3modified!!! 田代さんの指摘
prob4 <- logistic(beta[1] + beta[2] * 4)
cat(sprintf("prob4 = %.2f\n", prob4))
dev.on("plot3modified", width = width, height = width)
par.glmm()
d4 <- d[d$x == 4,]
plot(
	v.x, dbinom(v.x, 8, prob = prob4) * nrow(d4),
	type = "b",
	ylim = c(0, 6),
	pch = 20,
	xlab = "", ylab = ""
)
points(v.x, summary(as.factor(d4$y)), pch = 1)
dev.off()

# plot4
dev.on("plot4", width = width, height = height)
par.glmm()
plot.d(d, col = "black")
for (re in d$re) lines(
	xx, logistic(-4 + xx + re) * max(v.x),
	lwd = 1,
	col = "#00000020"
)
line.true()
dev.off()

for (j in 1:length(vr)) {
	dev.on(sprintf("plot4b%i", j), width = width, height = height)
	par.glmm()
	plot.d(d, col = NA, axes = FALSE)
	axis(1)
	box()
	if (j == 1) axis(2)
	lines(
		xx, logistic(-4 + xx + vr[j]) * max(v.x),
		lwd = 2,
		col = "#00000060"
	)
	points(4, 7)
	abline(v = 4, lty = 3)
	dev.off()
}

# RE, GLMM
library(glmmML)
d$id <- 1:nrow(d)
dev.on("plot5", width = width, height = height)
par.glmm()
plot.d(d, col = "black")
fitC <- glmmML(cbind(y, N - y) ~ x, cluster = d$id, data = d, family = binomial)
beta <- fitC$coefficients
line.true()
lines(xx, logistic(beta[1] + beta[2] * xx) * max(v.x), lwd = 3)
dev.off()
source("../hbm/gaussianbinom.R")
dev.on("plot5d4", width = width, height = width)
par.glmm()
d4 <- d[d$x == 4,]
plot(
	v.x,
	nrow(d4) * d.gaussian.binom(
		v.x, 8,
		fixed = beta[1] + beta[2] * 4,
		sd = fitC$sigma
	),
	type = "b",
	ylim = c(0, 6),
	pch = 20,
	xlab = "", ylab = ""
)
points(v.x, table(factor(d4$y, levels = v.x)), pch = 1)
dev.off()

