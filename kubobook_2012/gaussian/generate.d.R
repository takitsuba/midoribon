n.g <- 6
lambda <- c(3.5, 4.3)
d <- data.frame(
	f = factor(rep(c("C", "T"), each = n.g)),
	block = sapply(1:(2 * n.g), function(i) sprintf("block%02i", i))
)
m <- sapply(1:8, function(i) rpois(nrow(d), lambda = lambda[d$f]))
colnames(m) <- sapply(1:ncol(m), function(j) sprintf("y.%i", j))
d <- cbind(d, m)

d$y.mean <- apply(d, 1, function(Y) mean(as.numeric(Y[3:10])))

v <- sapply(1:8, function(j) sprintf("y.%i", j))
d.long <- reshape(d, direction = "long", varying = v)

save(d, d.long, file = "d.RData")
