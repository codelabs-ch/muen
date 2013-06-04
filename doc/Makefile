all: memlayout.pdf report.pdf

report.pdf: report.tex
	@pdflatex $<

memlayout.pdf: memlayout.tex
	@pdflatex $<

clean:
	@rm -f *.pdf *.aux *.log
