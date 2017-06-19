N.block <- 10 
N.rep <- 10
N.id <- N.block * N.rep

d <- data.frame(
	id = 1:N.id,
	pot = factor(LETTERS[rep(1:N.block, each = N.rep)]),
	f = factor(c("C", "T")[rep(1:2, each = N.id * 0.5)])
)

re.pot <- rnorm(N.block, 0, 1)
lambda <- exp(1 + re.pot[d$pot] + rnorm(N.id, 0, 1))
d$y <- rpois(N.id, lambda = lambda)

write.csv(d, file = "d1.csv", quote = FALSE, row.names = FALSE)
