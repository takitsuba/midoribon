get.Rhat <- function(m) # m: matrix
{
	N <- nrow(m) # number of iterations
	M <- ncol(m) # number of chains
	phi.j <- apply(m, 2, mean)
	phi <- mean(phi.j)
	B <- sum((phi.j - phi)^2) * N / (M - 1)
	s2.j <- apply(m, 2, var) * N / (N - 1)
	W <- mean(s2.j)
	varplus <- (N - 1) / N * W + B / N
	Rhat <- sqrt(varplus / W)
	c(Rhat, varplus)
}
