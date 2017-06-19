source("COMMON.R")

print.fit <- function(name, f)
{
	file <- sprintf("summary%s.txt", name)
	cat("# output to", file, "...\n")
	sink(file)
	print(summary(f))
	cat("\n\n")
	print(f)
	sink()
}

print.fit("NULL", fit.null)
print.fit("f", fit.f)
print.fit("x", fit)
print.fit("xf", fit.xf)
