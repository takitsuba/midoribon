N <- 30
x <- seq(0, 2, length = N)
m <- function(xx) exp(-4 + 3 * xx)

d <- data.frame(
	x = x,
	y = rpois(N, lambda = m(x))
)
#plot(d$x, d$y)
#lines(d$x, m(d$x))
