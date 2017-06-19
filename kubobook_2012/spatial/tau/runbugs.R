source("set.data.R")

set.param("re", matrix(rnorm(N.site * 3, 0, 0.1), 3, N.site))

post.bugs <- call.bugs(n.iter = 300, n.burnin = 100, n.thin = 1)
post.list <- to.list(post.bugs)
post.mcmc <- to.mcmc(post.bugs)
