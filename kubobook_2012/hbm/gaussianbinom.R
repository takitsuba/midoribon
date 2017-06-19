f.gaussian.binom <- function(alpha, x, size, fixed, sd)
	dbinom(x, size, logistic(fixed + alpha)) * dnorm(alpha, 0, sd)

d.gaussian.binom <- function(v.x, size, fixed, sd) sapply(
	v.x, function(x) integrate(
		f = f.gaussian.binom,
		lower = -sd * 10,
		upper = sd * 10,
		# for f.gaussian.binom
		x = x,
		size = size,
		fixed = fixed,
		sd = sd
	)$value
)
