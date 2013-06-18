PDFLATEX = for n in 1 2 3; do pdflatex $<; done

REPORT_SOURCES =       \
	report.tex         \
	report.bib         \
	introduction.tex   \
	background.tex     \
	design.tex         \
	implementation.tex \
	conclusion.tex     \
	images/*

all: memlayout.pdf report.pdf slides.pdf

report.pdf: $(REPORT_SOURCES)
	@pdflatex $<
	@makeindex report
	@bibtex report
	@pdflatex $<
	@pdflatex $<

slides.pdf: slides.tex
	@$(PDFLATEX)

memlayout.pdf: memlayout.tex
	@$(PDFLATEX)

clean:
	@rm -f *.pdf *.aux *.log *.toc *.snm *.out *.nav *.vrb *.bbl *.blg
