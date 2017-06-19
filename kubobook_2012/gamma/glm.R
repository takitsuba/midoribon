source("../COMMON.R")

load("d.RData")

fit <- glm(y ~ log(x), family = Gamma(link = "log"), data = d)
sum.fit <- summary(fit)
vc <- sum.fit$coefficients[,"Estimate"]
names(vc) <- c("b1", "b2")
phi <- sum.fit$dispersion # dispersion parameter
sink("glm.txt")
print(sum.fit)
sink()

width  <- 4 # inch
height <- 3 # inch

get.y.mean <- function(b1, b2, x) exp(b1 + b2 * log(x))
plot.d <- function(tline = TRUE)
{
	plot(d$x, d$y, xlab = "", ylab = "") 
	if (tline) {
		lines(d$x, get.y.mean(p[["b1"]], p[["b2"]], d$x), lty = 2, lwd = 2)
	}
}

dev.on("plot1", width = width, height = height, top = 1)
plot.d()
dev.off()

dev.on("plot2", width = width, height = height, top = 1)
plot.d(FALSE)
lines(d$x, get.y.mean(-1.3, 0.5, d$x), lty = 1, lwd = 2, col = "#808080")
lines(d$x, get.y.mean(-0.5, 2, d$x), lty = 1, lwd = 2, col = "#808080")
text(0.4, 0.4, "?", cex = 10, col = "#808080")
dev.off()

dev.on("plot3", width = width, height = height, top = 1)
plot.d()
lines(d$x, predict(fit, newdata = data.frame(x = d$x), type = "response"), lwd = 2)
m <- get.y.mean(vc["b1"], vc["b2"], d$x)
rate <- 1 / (phi * m)
shape <- 1 / phi
plot.pi <- function(q) polygon(
	c(d$x, rev(d$x)),
	c(qgamma(q, shape, rate), rev(qgamma(1 - q, shape, rate))),
	border = NA,
	col = "#00000020"
)
plot.pi(q = 0.05)
plot.pi(q = 0.25)
# median
lines(d$x, qgamma(0.5, shape, rate), col = "#808080", lwd = 2)

dev.off()
