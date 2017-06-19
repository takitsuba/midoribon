source("COMMON.R")

m.data <- matrix(
	rpois(n.sample * n.rep, m),
	n.sample, n.rep
)

v.mle <- apply(m.data, 2, mean)
o <- order(v.mle)
v.mle <- v.mle[o]
m.data <- m.data[,o]

m.mle <- sapply(
	v.mle,
	function(mle)
	{
		apply(
			m.data, 2, function(v.d)
			{
				sum(dpois(v.d, mle, log = TRUE))
			}
		)
	}
)

v.mean.mle <- apply(m.mle, 2, mean)

v.ll <- sapply(
	1:ncol(m.data),
	function(j) sum(dpois(m.data[,j], v.mle[j], log = TRUE))
)

file <- "data.RData"
cat("# output to", file, "...\n")
save(m.data, v.mle, m.mle, v.mean.mle, v.ll, file = file)
