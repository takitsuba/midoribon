pb <- function(d, n.bootstrap)
{
	n.sample <- nrow(d)
	y.mean <- mean(d$y)
	cat("# ")
	v.d.dev12 <- sapply(
		1:n.bootstrap,
		function(i) {
			cat(".")
			if (i %% 50 == 0) cat("\n# ")
			d$y.rnd <- rpois(n.sample, lambda = y.mean)
			fit1 <- glm(y.rnd ~ 1, data = d, family = poisson)
			fit2 <- glm(y.rnd ~ x, data = d, family = poisson)
			fit1$deviance - fit2$deviance
		}
	)
	cat("\n")
	v.d.dev12
}
