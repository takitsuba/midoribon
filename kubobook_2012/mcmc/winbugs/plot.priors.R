source("../../COMMON.R")

tau <- c(1, 1.0E-4)
sd <- 1 / sqrt(tau)

logistic <- function(z) 1 / (1 + exp(-z))

width  <- 3.0 # inch
height <- 2.4 # inch

dbeta <- 0.005
beta <- seq(-11, 11, dbeta * 2)

# prior for a
dev.on("priora", width = width, height = height)
plot(
	beta, dnorm(beta, 0, sd[1]),
	type = "l", 
	xlim = c(min(beta) + 1, max(beta) - 1),
	ylim = c(0, dnorm(0, 0, sd[1])),
	ylab = "",
	col = "#808080",
	lwd = 1,
	lty = 2
)
lines(
	beta, dnorm(beta, 0, sd[2]),
	lwd = 2
)
dev.off()

# prior for q: logit-normal
dnorm.logistic <- function(s)
{
	dq <- logistic(beta + dbeta) - logistic(beta - dbeta)
	z <- (pnorm(max(beta) + dbeta, 0, s) - pnorm(min(beta) - dbeta, 0, s))
	dnorm(beta, 0, s) / dq / z * dbeta * 2
}
dev.on("priorq", width = width, height = height)
plot(
	logistic(beta), dnorm.logistic(sd[1]),
	type = "l", 
	xlim = c(0, 1),
	ylim = c(0, dbeta(0.01, 0.5, 0.5)),
	ylab = "",
	col = "#808080",
	lwd = 1,
	lty = 2
)
lines(
	logistic(beta), dnorm.logistic(sd[2]),
	lwd = 2
)
dev.off()

# prior for q: uniform distribution
dev.on("unif", width = width, height = height)
plot(
	c(0, 0, 1, 1),
	c(0, 1, 1, 0),
	type = "l",
	lwd = 2,
	ylim = c(0, 2),
	xlab = "",
	ylab = ""
)
dev.off()

# prior for q: uniform distribution discrete
dev.on("unifD", width = width * 1.5, height = height)
plot(
	c(0, 0, 1, 1),
	c(0, 1, 1, 0),
	type = "n",
	ylim = c(0, 2),
	xlab = "",
	ylab = "",
	axes = F,
	yaxs = "i"
)
#rect(0, 0, 1, 1, col = "#808080", border = NA)
xx <- seq(0.01, 0.99, 0.01)
segments(xx, 0, xx, 0.8, lwd = 0.5)
abline(h = 0)
axis(1)
dev.off()

# prior for q: Jefferys' distribution
q <- seq(0, 1, 0.01)
dev.on("jefferys", width = width, height = height)
plot(
	q,
	dbeta(q, 0.5, 0.5),
	type = "l",
	lwd = 2,
	ylim = c(0, dbeta(0.01, 0.5, 0.5)),
	xlab = "",
	ylab = ""
)
dev.off()



