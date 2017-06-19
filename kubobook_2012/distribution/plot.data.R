# R --vanilla!
source("COMMON.R")

N <- length(data)

# histgram
dev.on("histdata", width = width, height = height, top = 1)
hist(data, breaks = seq(-0.5, 9.5))
dev.off()

# dpois
y <- 0:9
prob <- dpois(y, lambda = 3.56)
dev.on("dpois356", width = width, height = height, top = 1)
plot(y, prob, type = "b", lty = 2)
dev.off()

# hist & dpois
dev.on("dpoishistdata", width = width, height = height, top = 1)
hist(data, breaks = seq(-0.5, 9.5))
lines(y, prob * N, type = "b", lty = 2)
dev.off()

# dpoist.set
y <- 0:20
dev.on("dpoisset", width = width, height = height, top = 1)
plot(y, dpois(y, lambda = 3.5), type = "b", lty = 2, pch = 21, ylab = "prob")
lines(y, dpois(y, lambda = 7.7), type = "b", lty = 2, pch = 23)
lines(y, dpois(y, lambda = 15.1), type = "b", lty = 2, pch = 24)
legend("topright", legend = c(3.5, 7.7, 15.1), pch = c(21, 23, 24),
title = "lambda", cex = 0.7)
dev.off()

# dnorm
y <- seq(-5, 5, length = 100)
dev.on("dnorm", width = width, height = height, top = 0.1)
plot(y, dnorm(y, mean = 0, sd = 1), type = "l", xlab = "", ylab = "")
lines(y, dnorm(y, mean = 0, sd = 3), lty = 2)
lines(y, dnorm(y, mean = 2, sd = 1), lty = 3)
dev.off()

# dgamma
y <- seq(0.01, 5, length = 100)
dev.on("dgamma", width = width, height = height, top = 0.1)
plot(y, dgamma(y, 1, 1), type = "l", ylab = "prob.density")
lines(y, dgamma(y, 5, 5), lty = 2)
lines(y, dgamma(y, 0.1, 0.1), lty = 3)
dev.off()
