outdir := ./public

public: src content
	mkdir -p $(outdir)
	cp src/styles.css $(outdir)/styles.css
	stack run
