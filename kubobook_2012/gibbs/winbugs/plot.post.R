source("../../COMMON.R")
source("R2WBwrapper.R")

load("post.bugs.RData")

sink("post.txt")
print(post.bugs, digits.summary = 3)
sink()
dev.on("postbugs", 8, 8)
plot(post.bugs)
dev.off()

width  <- 8.0 # inch
height <- 3.2 # inch

# prior for a
plot.post <- function(key) {
	dev.on(paste("post", key, sep = ""), width = width, height = height)
	par(mar = c(2.5, 2.5, 0.1, 0.5), mgp = c(1.5, 0.5, 0), cex = 1.0)
	post.list <- to.list(post.bugs)
	post.mcmc <- to.mcmc(post.bugs)
	plot(
		post.list[,colnames(post.mcmc) %in% key,],
		col = "#00000060",
		lwd = 1,
		lty = 1,
		smooth = FALSE
	)
	dev.off()
}

plot.post("beta1")
plot.post("beta2")

