source("R2WBwrapper.R")
load("../data.RData")

clear.data.param()
set.data("Y", data)
set.data("N", length(data))
set.data("Size", 8)

set.param("beta", 0)

post.bugs <- call.bugs(
	file = "model.bug.txt",
	n.iter = 2100, n.burnin = 100, n.thin = 5
)
post.list <- to.list(post.bugs)
post.mcmc <- to.mcmc(post.bugs)
