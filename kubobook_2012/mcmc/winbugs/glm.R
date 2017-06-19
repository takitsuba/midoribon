load("../data.RData")
Y <- data

fit <- glm(cbind(Y, 8 - Y) ~ 1, family = binomial)
print(summary(fit))

