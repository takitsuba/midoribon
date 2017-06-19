d <- data.frame(
	y.0 = c(2, 3, 5, 4, 1, 2),
	y.1 = c(4, 2, 9, 4, 0, 4),
	x   = seq(0.1, 0.6, length = 6)
)
d$total <- d$y.0 + d$y.1
d.long <- reshape(d, direction = "long", varying = c("y.0", "y.1"))
cn <- colnames(d.long)
colnames(d.long)[grep("time", cn)] <- "response"
d.long$response <- factor(d.long$response)

fit.b1 <- glm(
	formula = cbind(y.1, y.0) ~ 1,
	data = d, family = binomial
)
fit.p1 <- glm(
	y ~ response, offset = log(total),
	data = d.long, family = poisson
)

fit.b2 <- glm(
	formula = cbind(y.1, y.0) ~ x,
	data = d, family = binomial
)
fit.p2 <- glm(
	y ~ response * x, offset = log(total),
	data = d.long, family = poisson
)
