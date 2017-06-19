source("../COMMON.R")

m <- 8
n.sample <- 50 # per estimation
n.rep <- 200   # number of estimations

j1 <- 48
j2 <- 182
vj1 <- c(j1, 1, 15, 129, j2, 198, 200)
vj2 <- c(vj1, 5, 33, 84, 113, 146, 169, 194)
vj12 <- c(vj1[2], vj2[7], vj2[4], vj1[6])

