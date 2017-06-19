source("../R2WBwrapper.R")
clear.data.param()

load("../Y.RData")

set.data("N.site", length(Y))
set.data("Y", matrix(c(Y, Y, Y), N.site, 3))

Adj <- c(sapply(2:(N.site - 1), function(a) c(a - 1, a + 1)))
set.data("Adj", c(2, Adj, N.site - 1))
set.data("Weights", rep(1, 2 * N.site - 2))
set.data("Num", c(1, rep(2, N.site - 2), 1))

set.data("Beta", 2.27)
set.data("Tau", c(1000, 20, 0.01))
