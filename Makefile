.PHONY: bench

MATCH=

bench:
	stack bench --ba '--json bench.json $(MATCH)'
	./plot.py
