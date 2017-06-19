d0 <- data.frame(
	Plot = factor(c(1, 1, 2, 2)),
	Spc = factor(c("A", "B", "A", "B")),
	y = c(52, 31, 27, 15)
)

d <- reshape(d0, timevar= "Spc", idvar = "Plot", direction = "wide")
rownames(d) <- 1:nrow(d)
print(d)

fit.b <- glm(cbind(y.B, y.A) ~ Plot, data = d, family = binomial)
print(fit.b)

d2 <- reshape(d, direction = "long", varying = c("y.A", "y.B"), timevar = "Spc")
d2$Spc <- factor(d2$Spc)
d2$id <- NULL
print(d2)

fit.p <- glm(y ~ Spc * Plot, data = d2, family = poisson)
print(fit.p)
