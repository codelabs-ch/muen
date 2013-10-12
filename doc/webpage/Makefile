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

SRC_FILES = \
	asciidoc.css \
	muen.svg \
	example.svg \
	muen.ipxe \
	set-feature-control-lock-bit.patch

DST_FILES = $(addprefix $(DESTDIR)/, $(SRC_FILES))

all: $(DESTDIR)/index.html $(DST_FILES)

$(DESTDIR)/%: %
	@cp $< $(DESTDIR)

$(DESTDIR)/index.html: index page.conf ../../README
	asciidoc $(ASCIIDOC_OPTS) $< > $@

clean:
	@rm -rf $(DESTDIR)/index.html $(DST_FILES)
