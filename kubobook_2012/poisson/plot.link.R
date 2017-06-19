source("COMMON.R")

mf <- 10
x.min <- 5
x.max <- 20 
xx <- seq(x.min, x.max, length = 50)
y <- function(x) exp(fit.xf$coefficients[1] + fit.xf$coefficients[2] * x)

plot.link <- function(file, yyC, yyT)
{
	dev.on(file, width = 3, height = 3, top = 0.1)
	plot(
		numeric(0), numeric(0), type = "n",
		xlim = c(x.min, x.max),
		ylim = c(y(x.min) * 0.7, y(x.max) * 1.05),
		xlab = "", ylab = ""
	)
	lines(xx, yyC, lwd = 2, col = "#000000")
	lines(xx, yyT, lwd = 2, col = "#808080")
	dev.off()
}

coef <- fit.xf$coefficients
plot.link(
	"linkLog",
	exp(coef[1] + coef[2] * xx),
	exp(coef[1] + coef[2] * xx + coef[3] * mf)
)

model.g <- glm(y ~ x + f, data = d, family = poisson(link = "identity"))
coef <- model.g$coefficients
plot.link(
	"linkIdentity",
	(coef[1] + coef[2] * xx),
	(coef[1] + coef[2] * xx + coef[3] * mf)
)

