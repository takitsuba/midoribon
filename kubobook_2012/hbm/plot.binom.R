# R --vanilla!
source("COMMON.R")
source("gaussianbinom.R")
source("R2WBwrapper.R")
load("post.bugs.RData")
post.mcmc <- to.mcmc(post.bugs)

d <- read.csv("data7a.csv")
logistic <- function(z) 1 / (1 + exp(-z))

cpdf <- function(file, width = 4, height = 3)
{
	cat("# output to", file, "...\n")
	CairoPDF(file, width = width, height = height)
	par(mar = c(2.5, 2.5, 0.1, 0.1), mgp = c(1.5, 0.5, 0))
}

plot.data <- function() plot(
	0:s, summary(as.factor(d$y)),
	ylim = range(c(0, dbinom(0:s, s, q) * n)),
	xlab = "",
	ylab = "",
	pch = 19
)

plot.polygon <- function() polygon(
	c(0:s, s:0),
	c(qp[1,], rev(qp[3,])),
	border = NA,
	col = "#00ffff80"
)

n <- nrow(d)
s <- 8
q <- sum(d$y) / (s * n)

# Bayes
a <- post.mcmc[, "beta"]
sigma <- post.mcmc[, "s"]
if (!exists("mp")) {
	cat("# generating mp ...\n")
	mp <- sapply(
		1:nrow(post.mcmc),
		function(i) n * d.gaussian.binom(
			0:s, s,
			fixed = a[i],
			sd = sigma[i]
		)
	)
}
qp <- apply(mp, 1, quantile, probs = c(0.025, 0.5, 0.975))


cpdf("post1.pdf")
plot.data()
plot.polygon()
lines(0:s, qp[2,], type = "b", col = "#00008080")
dev.off()

cpdf("post2.pdf")
plot.data()
lines(0:s, qp[2,], type = "b", col = "#00008080")
dev.off()

cpdf("post3.pdf")
plot.data()
lines(0:s, qp[2,], type = "b", col = "#00008080")
lines(
	0:s, dbinom(0:s, s, q) * n,
	type = "b", pch = 20,
	col = "#ff800080" # binomial
)
dev.off()


cpdf("adensplot", width = 6, height = 3)
par(mar = c(2.5, 2.5, 0.5, 0.5), mgp = c(1.5, 0.5, 0), cex = 1.5)
densplot(post.mcmc[,"beta"])
dev.off()


