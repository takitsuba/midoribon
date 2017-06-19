# R --vanilla!
source("COMMON.R")
fit2 <- glm(y ~ x, data = d, family = poisson)
fit1 <- glm(y ~ 1, data = d, family = poisson)
ll.full <-sum(dpois(d$y, lambda = d$y, log = TRUE))

# table
print.line <- function(name, ll, np, AIC = FALSE)
{
	cat(sprintf(
		"\\ttt{%s} & %i & %.1f & %.1f & %.1f",
		name, np, ll, -2 * ll, 2 * (ll.full - ll)
	))
	if (AIC) cat(sprintf("& %.1f", -2 * ll + 2 * np))
	cat("\\\\\n")
}

print.table <- function(file, AIC = FALSE)
{
	cat("# output to", file, "...\n")
	sink(file)
	print.line("Model 1", logLik(fit1), 1, AIC)
	print.line("Model 2", logLik(fit2), 2, AIC)
	print.line("FULL", ll.full, 100, AIC)
	sink()
}
print.table("table1.tex", AIC = TRUE)

width <- 4
height <- 4

# plot fit
dev.on("fit12", width = width * 0.8, height = height * 0.8, top = 0.0)
plot(
	d$x, d$y,
	col = "#aaaaaa",
	axes = FALSE,
	xlab = "", ylab = ""
)
axis(1)
axis(2)
par.usr <- par("usr")
abline(v = par.usr[1], lwd = 2)
abline(h = par.usr[3], lwd = 2)
xx <- seq(min(d$x), max(d$x), length = 50)
abline(h = mean(d$y), lty = 2, lwd = 2)
lines(xx, exp(1.29 + 0.0757 * xx), lwd = 2)
dev.off()

# small figures

plot.frame <- function(
	file,
	yy,
	xlim = c(7, 13),
	ylim = c(2, 15),
	xlab = "x",
	ylab = "",
	width = 2.0, height = 2.0
) {
	dev.on(file, width = width, height = height)
	par(mar = c(2.4, 1.5, 0.1, 0.1), mgp = c(1.3, 0.6, 0), cex = 1.2)
	plot(
		numeric(0), numeric(0),
		type = "n",
		axes = FALSE,
		xlab = xlab, ylab = ylab,
		xlim = xlim, ylim = ylim
	)
	axis(1)
	box()
	if (is.na(yy[1])) {
		points(d$x, d$y, col = "#00000060")
		abline(h = mean(d$y), lwd = 2)
	} else {
		points(d$x, yy, col = "#00000060")
	}
}

plot.frame("pois0", NA)
dev.off()
m <- mean(d$y)
n <- nrow(d)
for (i in 1:4) {
	plot.frame(sprintf("pois%i", i), rpois(n, m), xlab = "", ylab = "")
	dev.off()
}

