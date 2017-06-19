source("../COMMON.R")
d <- read.csv("data7a.csv")

q <- sum(d$y) / (8 * nrow(d))
logistic <- function(z) 1 / (1 + exp(-z))
