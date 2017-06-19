source("set.data.R")

set.param("beta", rep(0, 4))
set.param("r", matrix(rnorm(N.site * 4, 0, 0.1), 4, N.site))
set.param("s", rep(1, 4))

post.bugs <- call.bugs(n.iter = 10100, n.burnin = 100, n.thin = 10)
post.list <- to.list(post.bugs)
post.mcmc <- to.mcmc(post.bugs)
