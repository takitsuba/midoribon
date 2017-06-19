source("../../COMMON.R")
source("R2WBwrapper.R")

load("post.bugs.RData")
post.mcmc <- to.mcmc(post.bugs)
beta <- as.vector(post.mcmc[, "beta"])

width  <- 3.0 # inch
height <- 2.4 # inch

# histgram a
breaks <- seq(min(beta), max(beta), length = 30)
dev.on("hista", width = width, height = height)
hist(beta, breaks = breaks, main = "")
dev.off()

# histgram q
q <- 1 / (1 + exp(-a))
breaks <- seq(min(q), max(q), length = 30)
dev.on("histq", width = width, height = height)
hist(q, breaks = breaks, main = "")
dev.off()

sink("summaryq.txt")
cat("> summary(beta)\n\n")
print(summary(beta))
cat("> summary(q)\n\n")
print(summary(q))
cat("> 1 / (1 + exp(mean(-beta)))\n\n")
print(1 / (1 + exp(mean(-beta))))
sink()

