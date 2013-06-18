PDFLATEX = for n in 1 2 3; do pdflatex $<; done
XSLTPROC = xsltproc

XSLT_FILES = types.tex

REPORT_SOURCES =       \
	report.tex         \
	report.bib         \
	introduction.tex   \
	background.tex     \
	design.tex         \
	implementation.tex \
	conclusion.tex     \
	images/*

include ../Makeconf

all: memlayout.pdf report.pdf slides.pdf

report.pdf: $(REPORT_SOURCES) $(XSLT_FILES)
	@pdflatex $<
	@makeindex report
	@bibtex report
	@pdflatex $<
	@pdflatex $<

slides.pdf: slides.tex
	@$(PDFLATEX)

memlayout.pdf: memlayout.tex
	@$(PDFLATEX)

types.tex: xsl/xsd2tex-types.xsl
	$(XSLTPROC) -o $@ $< $(SCHEMA_DIR)/types.xsd

clean:
	@rm -f *.pdf *.aux *.log *.toc *.snm *.out *.nav *.vrb *.bbl *.blg
	@rm -f $(XSLT_FILES)
