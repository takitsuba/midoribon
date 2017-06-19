x <- seq(0.001, 0.8, length = 50)
get.y.mean <- function(b1, b2, x) exp(b1 + b2 * log(x))
p <- list(b1 = -1, b2 = 0.7, inv.phi = 3) # phi: dispersion parameter
y.mean <- get.y.mean(p[["b1"]], p[["b2"]], x)

d <- data.frame(
	x = x,
	y = rgamma(length(x), p[["inv.phi"]], p[["inv.phi"]] / y.mean)
)

save(d, p, file = "d.RData")
