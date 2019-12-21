all: slides.pdf

slides.toc:
	lualatex slides.tex

slides.pdf: slides.toc
	lualatex slides.tex

clean:
	rm -f slides.aux slides.log slides.nav \
	slides.out slides.toc slides.snm
