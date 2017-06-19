source("COMMON.R")
load("data.RData")
# v.mle: maximum likelihood estimate
# m.mle: log L*

plot.frame <- function(
	file,
	xlim = c(log(m - 1), log(m + 1)),
	ylim = c(-130, -119.5),
	ylab = "log likelihood",
	width = 4, height = 4
) {
	dev.on(file, width = width, height = height)
	par(mar = c(2.5, 2.5, 0.4, 0.4), mgp = c(1.5, 0.5, 0))
	plot(
		c(), c(),
		type = "n",
		xlab = "", 
		ylab = ylab,
		#las = 1,
		xlim = xlim, ylim = ylim
	)
	#abline(v = log(m), lty = 2)
}

add.mean.ll <- function(col = "#808080")
{
	lines(unique(log(v.mle)), unique(v.mean.mle), lwd = 2, lty = 2, col = col)
}
add.random.mle.points <- function(j, points = TRUE)
{
	if (points) points(
		rep(log(v.mle[j]), nrow(m.mle)), m.mle[,j],
		pch = "-",
		col = "#000000",
	)
	points(log(v.mle[j]), v.mean.mle[j], pch = 19, col = "#808080", cex = 1.5)
}
add.mle.point <- function(j)
{
	points(
		log(v.mle[j]), v.ll[j],
		pch = 21, bg = "#ffffff",
		cex = 1.5
	)
}
add.arrow<- function(j, dx = 0)
{
	y1 <- v.ll[j]
	y2 <- v.mean.mle[j]
	dy <- (y1 - y2) * 0.09
	arrows(
		log(v.mle[j]) + dx, y1 - dy,
		log(v.mle[j]) + dx, y2 + dy,
		length = 0.10,
		angle = 20,
		lwd = 1,
		lend = "round",
		col = "#808080"
	)
}
add.ll.curve <- function(j)
{
	vy <- sapply(v.mle, function(mle) sum(dpois(m.data[,j], mle, log = TRUE)))
	lines(log(v.mle), vy, lwd = 1)
}


# model j: an exmaple
plot.frame("llmlej")
add.ll.curve(j = j1)
add.mle.point(j = j1)
abline(h = v.ll[j1], lty = 2, col = "#808080")
dev.off()

# validation of model j
plot.frame("llmles1", ylim = c(-140, -110), height = 5)
add.ll.curve(j = j1)
add.random.mle.points(j = j1)
add.mle.point(j = j1)
add.arrow(j = j1, dx = 0.08)
abline(h = v.ll[j1], lty = 2, col = "#808080")
abline(h = v.mean.mle[j1], lty = 2, col = "#808080")
dev.off()

plot.frame("llmles2", ylim = c(-140, -110), height = 5)
add.ll.curve(j = j2)
add.random.mle.points(j = j2)
add.mle.point(j = j2)
abline(h = v.ll[j2], lty = 2, col = "#808080")
abline(h = v.mean.mle[j2], lty = 2, col = "#808080")
add.arrow(j = j2, dx = 0.08)
dev.off()

plot.frame("llmles3", ylim = c(-140, -110), height = 5, ylab = "")
#add.mean.ll()
for (jj in vj2) {
	add.random.mle.points(j = jj)
	add.mle.point(j = jj)
}
dev.off()

# delta
plot.frame("lldelta2", ylim = c(-140, -110), height = 5, ylab = "")
add.mean.ll()
for (jj in vj2) {
	add.random.mle.points(j = jj, points = FALSE)
	add.arrow(j = jj)
	add.mle.point(j = jj)
}
dev.off()

plot.frame("lldelta1")
add.ll.curve(j = j1)
add.mle.point(j = j1)
add.mean.ll()
add.random.mle.points(j = j1, points = FALSE)
add.arrow(j = j1)
dev.off()

