size <- 8
q.true <- 0.45
N.sample <- 20

file <- "./data.RData"
if (!file.exists(file)) {
	cat("# generating data...\n")
	data <- rbinom(N.sample, size = size, prob = q.true)
	save(data, file = file)
}

load("./data.RData")
source("../COMMON.R")

logL.binom <- function(qq)
{
	sum(dbinom(data, size, qq, log = TRUE))
}

q.hat <- mean(data) / size
delta.l <- 0.01
vq <- seq(0.25, 0.65, delta.l)
vlogL <- sapply(vq, logL.binom)

#density.logL <- exp(vlogL) / sum(exp(vlogL)) / delta.l
density.logL <- exp(vlogL) / sum(exp(vlogL)) # q が離散だから

i.start <- 6

