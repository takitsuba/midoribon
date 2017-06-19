source("COMMON.R")
fit.x <- glm(cbind(y, N - y) ~ x, data = d, family = binomial)
fit.f <- glm(cbind(y, N - y) ~ f, data = d, family = binomial)
fit.xf <- glm(cbind(y, N - y) ~ x + f, data = d, family = binomial)

fit.xfi <- glm(cbind(y, N - y) ~ x * f, data = d, family = binomial)

fit.null <- glm(cbind(y, N - y) ~ 1, data = d, family = binomial)
ll.full <- sum(dbinom(d$y, d$N, prob = d$y / d$N, log = TRUE))

