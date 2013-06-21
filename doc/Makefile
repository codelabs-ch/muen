PDFLATEX = for n in 1 2 3; do pdflatex $<; done
XSLTPROC = xsltproc

XSLT_FILES =       \
	binary.tex     \
	hardware.tex   \
	kernel.tex     \
	scheduling.tex \
	subject.tex    \
	types.tex

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

%.tex: $(SCHEMA_DIR)/%.xsd xsl/xsd2tex-types.xsl
	$(XSLTPROC) -o $@ xsl/xsd2tex-types.xsl $<

clean:
	@rm -f *.pdf *.aux *.lof *.log *.toc *.snm *.out *.nav *.vrb *.bbl *.blg
	@rm -f *.idx *.ilg *.ind
	@rm -f $(XSLT_FILES)
