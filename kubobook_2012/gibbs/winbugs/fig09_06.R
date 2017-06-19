library(R2WinBUGS)
if (!exists("d")) load("d.RData") 
if (!exists("post.bugs")) load("post.bugs.RData") 
#post.mcmc <- to.mcmc(post.bugs)
post.mcmc <- as.mcmc(post.bugs$sims.matrix)
beta1 <- post.mcmc[, "beta1"]
beta2 <- post.mcmc[, "beta2"]

add.mean <- function(bb1, bb2, lty = 2, lwd = 1, ...)
{
	lines(d$x, exp(bb1 + bb2 * (d$x - mean(d$x))), lty = lty, lwd = lwd, ...)
}

# fig09_06 (A)
par(ask = TRUE)
plot(d$x, d$y, type = "n", xlab = "x_i", ylab = "y_i")
for (i in 1:nrow(post.mcmc)) {
	add.mean(beta1[i], beta2[i], lty = 1, col = "#00000004")
}
points(d$x, d$y)
add.mean(median(beta1), median(beta2), lty = 1, lwd = 2)

# fig09_06 (B)
plot(
	as.matrix(post.mcmc)[,c("beta1", "beta2")],
	lty = 1, col = "#00000030",
	pch = 16, cex = 2,
	xlab = "", ylab = ""
)
