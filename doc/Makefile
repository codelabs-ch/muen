all: memlayout.pdf report.pdf slides.pdf

report.pdf: report.tex
	@pdflatex $<

slides.pdf: slides.tex
	@pdflatex $<

memlayout.pdf: memlayout.tex
	@pdflatex $<

clean:
	@rm -f *.pdf *.aux *.log *.toc *.snm *.out *.nav
