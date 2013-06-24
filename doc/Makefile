PDFLATEX = for n in 1 2 3; do pdflatex $<; done
XSLTPROC = xsltproc

XSLT_FILES =       \
	binary.tex     \
	hardware.tex   \
	kernel.tex     \
	scheduling.tex \
	subject.tex    \
	types.tex

COMMON_SOURCES =            \
	tikzstyle.tex           \
	graph_arch_overview.tex \
	graph_arch_example.tex  \
	graph_scheduler.tex     \
	images/*

REPORT_SOURCES =       \
	report.tex         \
	report.bib         \
	introduction.tex   \
	background.tex     \
	design.tex         \
	implementation.tex \
	impl_policy.tex    \
	impl_subject.tex   \
	conclusion.tex     \
	$(COMMON_SOURCES)

SLIDES_SOURCES =      \
	slides.tex        \
	$(COMMON_SOURCES)

include ../Makeconf

all: memlayout.pdf report.pdf slides.pdf

report.pdf: $(REPORT_SOURCES) $(XSLT_FILES)
	@pdflatex $<
	@makeindex report
	@bibtex report
	@pdflatex $<
	@pdflatex $<

slides.pdf: $(SLIDES_SOURCES)
	@$(PDFLATEX)

memlayout.pdf: memlayout.tex
	@$(PDFLATEX)

%.tex: $(SCHEMA_DIR)/%.xsd xsl/xsd2tex-types.xsl
	$(XSLTPROC) -o $@ xsl/xsd2tex-types.xsl $<

clean:
	@rm -f *.pdf *.aux *.lof *.log *.toc *.snm *.out *.nav *.vrb *.bbl *.blg
	@rm -f *.idx *.ilg *.ind
	@rm -f $(XSLT_FILES)
