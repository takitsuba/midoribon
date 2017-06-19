source("R2WBwrapper.R")
d <- read.csv("data7a.csv")

clear.data.param()
set.data("N", nrow(d))
set.data("Y", d$y)

set.param("beta", 0)
set.param("r", rnorm(N, 0, 0.1))
set.param("s", 1)
set.param("q", NA)

post.bugs <- call.bugs(
	file = "model.bug.txt",
	n.iter = 10100, n.burnin = 100, n.thin = 10
)
#post.list <- to.list(post.bugs)
#post.mcmc <- to.mcmc(post.bugs)
