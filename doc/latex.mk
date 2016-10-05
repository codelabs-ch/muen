PDFLATEX = for n in 1 2 3; do pdflatex $(DOC); done

SOURCES = \
	*.tex \
	../images/*

all: $(DOC).pdf

$(DOC).pdf: $(SOURCES)
	@$(PDFLATEX)

clean:
	@rm -f *.aux *.lof *.log *.lol *.lot *.out *.pdf *.toc
