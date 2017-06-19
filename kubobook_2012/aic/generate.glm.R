source("COMMON.R")
load("data.RData") # m.data

m.x <- matrix(
	rnorm(n.sample * n.rep, 0, 1),
	n.sample, n.rep
)

m.fit <- sapply(
	1:ncol(m.x), function(j) {
		x <- m.x[,j]
		y <- m.data[,j]
		fit1 <- glm(y ~ 1, family = poisson)
		fit2 <- glm(y ~ x, family = poisson)
		c(
			c(logLik(fit1)), # m.fit[1,]
			c(logLik(fit2)), # m.fit[2,]
			as.vector(fit1$coefficients), # m.fit[3:4,]
			as.vector(fit2$coefficients)  # m.fit[5,]
		)
	}
)

m.mll<- sapply(
	1:ncol(m.data),
	function(j) {
		m1 <- exp(m.fit[3, j])
		m2 <- exp(m.fit[4, j] + m.fit[5, j] * m.x[,j])
		m.ll <- sapply(
			1:ncol(m.data),
			function(jj) c(
				sum(dpois(m.data[,jj], m1, log = TRUE)),
				sum(dpois(m.data[,jj], m2, log = TRUE))
			)
		)
		apply(m.ll, 1, mean)
	}
)

file <- "glm.RData"
cat("# output to", file, "...\n")
save(m.x, m.fit, m.mll, file = file)
