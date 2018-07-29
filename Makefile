.PHONY: bench plot

MATCH=

#PLOT_FORMAT=interactive
PLOT_FORMAT=svg

PLOTS=all
#PLOTS=summary

bench:
	stack bench --ba '--json bench.json $(MATCH)'
	make plot PLOTS=$(PLOTS) PLOT_FORMAT=$(PLOT_FORMAT)

plot:
	PLOT_FORMAT=$(PLOT_FORMAT) ./plot.py $(PLOTS)
