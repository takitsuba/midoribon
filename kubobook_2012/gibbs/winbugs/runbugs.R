source("R2WBwrapper.R")
load("d.RData")

clear.data.param()
set.data("N", nrow(d))
set.data("Y", d$y)
set.data("X", d$x)
set.data("Mean.X", mean(d$x))

set.param("beta1", 0)
set.param("beta2", 0)

post.bugs <- call.bugs(
	file = "model.bug.txt",
	n.iter = 1600, n.burnin = 100, n.thin = 3
)
post.mcmc <- to.mcmc(post.bugs)
