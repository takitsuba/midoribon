generate.steps <- function(
	i = i.start,
	step.max = 100,
	n.thin = 1,
	optim = FALSE
) {
	total <- step.max * n.thin
	v.jump <- sapply(runif(total, 0, 1), function(p) ifelse(p > 0.5, -1, 1))
	v.rnd <- runif(total, 0, 1)
	i.sampled <- rep(0, step.max) # filling with dummy values
	i.min <- 1
	i.max <- length(vlogL)
	for (step in 1:step.max) {
		i.sampled[step] <- i
		for (k in 1:n.thin) {
			kk <- (step - 1) * n.thin + k
			i.next <- i + v.jump[kk]
			if (i.next < i.min) {
				i.next <- i.min + 1
			} else if (i.next > i.max) {
				i.next <- i.max - 1
			}
			p12 <- 0.5 # prob(i -> i.next)
			p21 <- 0.5 # prob(i.next -> i)
			if (i == i.min | i == i.max) {
				p12 <- 1
			} else  if (i == i.min + 1 & i.next == i.min) {
				p21 <- 1
			} else  if (i == i.max - 1 & i.next == i.max) {
				p21 <- 1
			}
			p <- exp(vlogL[i.next] + log(p21) - vlogL[i] - log(p12))
			if (optim & p < 1) {
				p <- 0
			}
			if (p > v.rnd[kk]) {
				i <- i.next
			}
		}
	}
	i.sampled
}
