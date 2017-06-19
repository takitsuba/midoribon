source("../COMMON.R")
d <- read.csv("data3a.csv")

fit <- glm(y ~ x, data = d, family = poisson)
fit.f <- glm(y ~ f, data = d, family = poisson)
fit.xf <- glm(y ~ x + f, data = d, family = poisson)
fit.null <- glm(y ~ 1, data = d, family = poisson)
ll.all <-sum(dpois(d$y, lambda = d$y, log = TRUE))

