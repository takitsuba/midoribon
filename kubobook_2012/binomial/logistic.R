source("COMMON.R")

width  <- 3.5 # inch
height <- 2.8 # inch

# z
dev.on("logistic", width = width, height = height, top = 1)
logistic <- function(z) 1 / (1 + exp(-z))
z <- seq(-6, 6, 0.1)
plot(z, logistic(z), type = "l", lwd = 2,
ylim = c(0, 1), yaxs = "i", xlab = "", ylab = "")
abline(v = 0, lty = 2)
dev.off()

# beta1 + beta2 * x
width  <- 8.0
height <- 3.2

xx <- seq(-3, 3, 0.1)
dev.on("logistic_beta12", width = width, height = height, top = 1)
par(mfrow = c(1, 2))
## panel 1
plot(xx, logistic(0 + 2 * xx), type = "l", lwd = 2,
ylim = c(0, 1), yaxs = "i", xlab = "", ylab = "",
main = "")
lines(xx, logistic(2 + 2 * xx), lwd = 2, col = "gray")
lines(xx, logistic(-3 + 2 * xx), lwd = 2, col = "gray")
#legend("topleft", legend = c(0, 2, -3), lty = c("solid", "dashed", "28"),
#lwd = 2, title = "", cex = 1.3)
## panel 2
plot(xx, logistic(0 + 2 * xx), type = "l", lwd = 2,
ylim = c(0, 1), yaxs = "i", xlab = "", ylab = "",
main = "")
lines(xx, logistic(0 + 4 * xx), lwd = 2, col = "gray")
lines(xx, logistic(0 - 1 * xx), lwd = 2, col = "gray")
#legend("left", legend = c(2, 4, -1), lty = c("solid", "dashed", "28"),
#lwd = 2, title = "", cex = 1.3)
dev.off()
