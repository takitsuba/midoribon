here <- getwd()
setwd("..")
source("COMMON.R")
setwd(here)

source("R2WBwrapper.R")
load("post.bugs.RData")
post.mcmc <- to.mcmc(post.bugs)

plot.frame("lambda", width = 3, height = 3)
beta1 <- post.mcmc[, "beta1"]
beta2 <- post.mcmc[, "beta2"]
for (i in 1:nrow(post.mcmc)) {
	add.mean(beta1[i], beta2[i], lty = 1, col = "#00000004")
}
add.xy()
add.mean(median(beta1), median(beta2), lty = 1, lwd = 2)
dev.off()

# beta1 and beta2
dev.on("beta12", width = 3, height = 3, top = 0.2)
plot(
	as.matrix(post.mcmc)[,c("beta1", "beta2")],
	lty = 1, col = "#00000030",
	pch = 16, cex = 0.6,
	xlab = "", ylab = ""
)
dev.off()
