source("../COMMON.R")

tau <- c(1, 1.0E-4)
sd <- 1 / sqrt(tau)

logistic <- function(z) 1 / (1 + exp(-z))

width  <- 3.0 # inch
height <- 2.4 # inch

dbeta <- 0.005
beta <- seq(-11, 11, dbeta * 2)

# prior for a
dev.on("priorBeta", width = width, height = height)
par(mar = c(2.5, 1.5, 0.4, 0.1), mgp = c(1.5, 0.5, 0), cex = 1.0)
plot(
	beta, dnorm(beta, 0, sd[1]),
	type = "l", 
	xlim = c(min(beta) + 1, max(beta) - 1),
	ylim = c(0, dnorm(0, 0, sd[1])),
	xlab = "", ylab = "",
	col = "#808080",
	lwd = 1,
	lty = 2
)
lines(
	beta, dnorm(beta, 0, sd[2]),
	lwd = 2
)
dev.off()

