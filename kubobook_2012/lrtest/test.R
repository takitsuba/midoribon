source("COMMON.R")
fit1 <- glm(y ~ 1, data = d, family = poisson)
fit2 <- glm(y ~ x, data = d, family = poisson)

load("diff.dev12.RData") # diff.dev12.pb

width <- 6
height <- 3

dev.on("hist12", width = width, height = height)
par(mar = c(1.5, 1.5, 0.1, 0.1), mgp = c(1.5, 0.5, 0))
h <- hist(diff.dev12, 100, plot = FALSE)
plot(
	h,
	main = "",
	xlab = "", ylab = ""
)
diff.dev12.obs <- fit1$deviance - fit2$deviance
abline(v = diff.dev12.obs, lty = 2)
dev.off()

cat(sprintf("# diff.dev12.obs = %.2f\n", diff.dev12.obs))
cat(sprintf(
	"# prob(>%.2f) = %i / 1000\n",
	diff.dev12.obs, sum(diff.dev12 > diff.dev12.obs)
))
