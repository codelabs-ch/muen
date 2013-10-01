DESTDIR   ?= html
STYLEPATH ?= .
PREPARE   := $(shell mkdir -p $(DESTDIR))

ASCIIDOC_OPTS = \
	--backend=xhtml11 \
	--conf-file=page.conf \
	-a stylesdir=$(STYLEPATH) \
	-a linkcss \
	-a disable-javascript \
	-o -

all: $(DESTDIR)/asciidoc.css $(DESTDIR)/muen.png $(DESTDIR)/index.html

$(DESTDIR)/asciidoc.css: asciidoc.css
	cp $< $@

$(DESTDIR)/muen.png: muen.png
	cp $< $@

$(DESTDIR)/index.html: index page.conf ../../README
	asciidoc $(ASCIIDOC_OPTS) $< > $@

clean:
	@rm -rf $(DESTDIR)/*.html
	@rm -rf $(DESTDIR)/*.css
	@rm -rf $(DESTDIR)/*.png
