source("R2WBwrapper.R")
clear.data.param()

load("Y.RData")
no <- c(6, 9, 12, 13, 26:30)
Yno <- Y
Yno[no] <- NA

set.data("N.site", length(Y))
set.data("Y", matrix(c(Y, Yno, Y, Yno), N.site, 4))

Adj <- c(sapply(2:(N.site - 1), function(a) c(a - 1, a + 1)))
set.data("Adj", c(2, Adj, N.site - 1))
set.data("Weights", rep(1, 2 * N.site - 2))
set.data("Num", c(1, rep(2, N.site - 2), 1))

