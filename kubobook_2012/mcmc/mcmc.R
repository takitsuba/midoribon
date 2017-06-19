.1ource("COMMON.R")
source("generate.steps.R")

df.mcmc <- function(i = i.start, step.max, n.thin, ...)
{
	cat("# sampling under step.max =", step.max, " n.thin =", n.thin, "\n")
	sample <- generate.steps(i, step.max = step.max, n.thin = n.thin, ...)
	data.frame(step = seq(1, step.max * n.thin, n.thin), sample = sample)
}

output.optim1 <- df.mcmc(i = i.start, step.max = 100, n.thin = 1, optim = TRUE)
output.optim2 <- df.mcmc(i = i.start + 30, step.max = 100, n.thin = 1, optim = TRUE)
output.mcmc1 <- df.mcmc(step.max = 100, n.thin = 1)
output.mcmc2 <- df.mcmc(step.max = 1000, n.thin = 1)
output.mcmc3 <- df.mcmc(step.max = 1000, n.thin = 100)

file <- "output.mcmc.RData"
cat("# output to", file, "...\n")
save(output.optim1, output.optim2, output.mcmc1, output.mcmc2, output.mcmc3, file = file)



