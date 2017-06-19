source("R2WBwrapper.R")
d <- read.csv("d1.csv")

clear.data.param()
set.data("N.sample", nrow(d))
set.data("N.pot", length(levels(d$pot)))
set.data("N.tau", 2)

set.data("Y", d$y)
set.data("F", as.numeric(d$f == "T"))
set.data("Pot", as.numeric(d$pot))

set.param("beta1", 0)
set.param("beta2", 0)
set.param("s", c(1, 1))
set.param("r", rnorm(N.sample, 0, 0.1))
set.param("rp", rnorm(N.pot, 0, 0.1))

post.bugs <- call.bugs(
	file = "model.bug.txt",
	n.iter = 51000, n.burnin = 1000, n.thin = 50
)
#post.list <- to.list(post.bugs)
#post.mcmc <- to.mcmc(post.bugs)
