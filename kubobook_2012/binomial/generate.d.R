n <- 100
N <- 8
x <- round(rnorm(n, 10, 1), 2)
f <- as.factor(c(rep("C", 50), rep("T", 50)))
xf <- as.numeric(f) - 1 # c(C = 0, T = 1)
d <- data.frame(
	N = N,
	y = rbinom(n, N, prob = 1 / (1 + exp(20 - 2 * x - 2 * xf))),
	x = x,
	f = f
)
file <- "data4a.csv"
cat("# output to", file, "...\n")
write.csv(d, file = file, quote = FALSE, row.names = FALSE)
