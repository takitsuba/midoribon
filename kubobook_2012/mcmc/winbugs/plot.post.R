source("../../COMMON.R")
source("R2WBwrapper.R")

load("post.bugs.RData")

sink("post.txt")
print(post.bugs, digits.summary = 3)
sink()

width  <- 6.0 # inch
height <- 2.4 # inch

# prior for a
dev.on("posta", width = width, height = height)
post.list <- to.list(post.bugs)
post.mcmc <- to.mcmc(post.bugs)
plot(
	post.list[,colnames(post.mcmc) %in% "beta",],
	col = "#00000060",
	lwd = 1,
	lty = 1,
	smooth = FALSE
)
dev.off()

