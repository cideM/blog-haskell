outdir := ./public

public: src
	mkdir -p $(outdir)
	cp src/styles.css $(outdir)/styles.css
	stack run
