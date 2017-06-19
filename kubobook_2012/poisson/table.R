source("COMMON.R")

print.line <- function(name, ll, np, AIC = FALSE)
{
	cat(sprintf(
		"\\ttt{%s} & %i & %.1f & %.1f & %.1f",
		name, np, ll, -2 * ll, 2 * (ll.all - ll)
	))
	if (AIC) cat(sprintf("& %.1f", -2 * ll + 2 * np))
	cat("\\\\\n")
}

print.table <- function(file, AIC = FALSE)
{
	cat("# output to", file, "...\n")
	sink(file)
	print.line("切片だけ", logLik(fit.null), 1, AIC)
	print.line("f", logLik(fit.f), 2, AIC)
	print.line("x", logLik(fit), 2, AIC)
	print.line("x + f", logLik(fit.xf), 3, AIC)
	print.line("フル", ll.all, 100, AIC)
	sink()
}
print.table("table1.tex", AIC = FALSE)
print.table("table2.tex", AIC = TRUE)

