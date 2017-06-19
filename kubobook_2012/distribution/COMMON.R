load("data.RData")
source("../COMMON.R")

width  <- 3.5 # inch
height <- 2.8 # inch

logL.pois <- function(lambda)
{
	sum(dpois(data, lambda, log = TRUE))
}
