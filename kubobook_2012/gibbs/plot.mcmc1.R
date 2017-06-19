here <- getwd()
setwd("../mcmc/")
source("COMMON.R")
setwd(here)

width  <- 4 # inch
height <- 3.2 # inch

da <- 0.01
va <- seq(-0.8, 0.8, da)
vpost <- exp(sapply(
	va, function(a) sum(
		dbinom(data, size, prob = 1 / (1 + exp(-a)), log = TRUE)
	) + dnorm(a, 0, 1.0e+2, log = TRUE) - log(da)
))
vpost <- vpost / sum(vpost)

cex <- 1.2
# likelihood -> posterior
dev.on("mcmc1a", width = width, height = height)
par(cex = cex)
plot(
	va, vpost,
	type = "l",
	lwd = 2,
	xlab = "beta",
	ylab = "",
	axes = FALSE,
	ylim = c(0, max(vpost) * 1.3)
)
v.at <- seq(min(va), max(va), 0.2)
axis(1, at = v.at)
dev.off()

# beta table
source("./R2WBwrapper.R")
load("../mcmc/winbugs/post.bugs.RData")
beta <- (to.mcmc(post.bugs))[,"beta"]
n <- 8

print.beta <- function(i, j) {
	cat(sprintf(
		"\\put(%i,%i){$\\beta = % .4f$}\n",
		540 + (j - 1) * 210,
		400 - (i - 1) * 50,
		beta[(j - 1) * n + i]
	))
}

file <- "mcmc1b.tex"
cat("# output to", file, "...\n")
sink(file)
for (j in 1:2) {
	for (i in 1:n) print.beta(i, j)
}
sink()


