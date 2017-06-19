include ../common.mk

pdf: plot.data.R wald.R
	for f in $<; do R --vanilla < $$f; done
	${MAKE} ebb

