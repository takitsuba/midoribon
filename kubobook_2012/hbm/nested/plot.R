# R --vanilla!
library(Cairo)
source("R2WBwrapper.R")
load("post.bugs.RData")
post.mcmc <- to.mcmc(post.bugs)
post.list <- to.list(post.bugs)
d <- read.csv("d1.csv")

dev.on <- function(file, width = 4, height = 4, bg = "transparent")
{
	cat("# output to", file, "...\n")
	CairoPDF(
		file = file,
		paper = "special",
		bg = bg,
		width = width,
		height = height,
		family = "Japan1"
	)
	par(mar = c(2.5, 2.5, 0.1, 0.1), mgp = c(1.5, 0.5, 0))
}

# data
dev.on("data1.pdf")
plot(
	d$id, d$y, pch = as.character(d$pot),
	ylim = c(0, 44),
	col = c("#000000", "#808080")[d$f], xlab = "", ylab = ""
)
dev.off()
dev.on("data1m.pdf")
plot(
	d$id, d$y, pch = as.character(d$pot),
	ylim = c(0, 44),
	col = c("#000000", "#808080")[d$f], xlab = "", ylab = ""
)
m <- mean(d[d$f == "C", "y"])
lines(c(0, 50), c(m, m), col = "#000000", lwd = 2, lty = 2)
m <- mean(d[d$f == "T", "y"])
lines(c(50, 100), c(m, m), col = "#808080", lwd = 2, lty = 2)
dev.off()
dev.on("data2.pdf")
plot(
	d$pot, d$y,
	ylim = c(0, 44),
	col = rep(c("#ffffff", "#808080"), each = 5)
)
dev.off()



# plot(post.bugs)
dev.on("plotbugs.pdf", width = 6, height = 6)
plot(post.bugs)
dev.off()

dev.on("posta.pdf", width = 6, height = 3)
plot(post.list[,1,], smooth = F) # "beta1"
dev.off()
dev.on("postb.pdf", width = 6, height = 3)
plot(post.list[,2,], smooth = F) # "beta2"
dev.off()

cnm <- colnames(post.mcmc)
m <- post.mcmc[,grep("rp", cnm)]
keys <- colnames(m)
list.density <- lapply(
	keys, function(key) {
		xy <- density(m[,key])
		list(x = xy$x, y = xy$y)
	}
)
names(list.density) <- keys
ymax <- max(sapply(list.density, function(xy) xy$y))
dymax <- 0.05
dev.on("postrp.pdf", width = 5, height = 2.5)
par(mar = c(2.5, 0.1, 0.1, 0.1), mgp = c(1.5, 0.5, 0))
plot(
	numeric(0), numeric(0),
	type = "n",
	xlim = quantile(m, probs = c(0.01, 0.99)),
	ylim = c(0, ymax * (1 + dymax * 2)),
	axes = FALSE,
	yaxs = "i",
	xlab = "", ylab = ""
)
abline(h = 0)
axis(1)
for (i in 1:10) {
	xy <- list.density[[i]]
	col <- c("#0000ff80", "#ff000080")[(i > 5) + 1]
	lines(xy$x, xy$y, lwd = 3, col = col)
	text(mean(xy$x), max(xy$y) + ymax * dymax, LETTERS[i], col = col)
}
dev.off()



