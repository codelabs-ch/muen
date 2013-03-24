#
# Copyright (c) 2009 Reto Buerki <buerki@swiss-it.ch>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#

# project settings

PROJECT := example-project
EMAIL   := developer@example.net

FORMATS := html txt pdf
TARGETS := $(foreach fmt,$(FORMATS),$(PROJECT).$(fmt))

SRCDIR  := ../../results
DOCDIR  := docs
XSLDIR  := xsl

SRCINDEX := $(SRCDIR)/index.xml

RESULTFILE := result.xml
HEADERFILE := result-header.xml
OVERFILE   := overview.xml
ENTITYFILE := data.ent

SOURCES := $(wildcard $(SRCDIR)/*.xml) \
	$(ENTITYFILE) \
	$(RESULTFILE) \
	$(HEADERFILE) \
	$(OVERFILE)

TRANSXSL := $(XSLDIR)/transform.xsl
OVERXSL  := $(XSLDIR)/overview.xsl
HTMLXSL  := $(XSLDIR)/html.xsl
TXTXSL   := $(XSLDIR)/txt.xsl

EXT := docbook

# testresult input produced by ahven xml runner

TESTS := `cd $(SRCDIR); ls TEST-*.xml`

# testenv info

TIME        := $(shell date)
OS          := $(shell uname)
HOSTNAME    := $(shell hostname)
KERNEL_NAME := $(shell uname -s)
KERNEL_REL  := $(shell uname -r)
KERNEL_VER  := $(shell uname -v)

DBLATEX := dblatex --style=db2latex
XP      := xsltproc --nonet --novalid --xinclude

all: $(TARGETS)

$(PROJECT).html: $(SOURCES)
	@echo -n "Generating $@ ... "
	@$(XP) $(HTMLXSL) index.xml > $@
	@echo "DONE"

$(PROJECT).txt: $(SOURCES)
	@echo -n "Generating $@ ... "
	@$(XP) $(TXTXSL) index.xml | w3m -cols 65 -dump -T text/html > $@
	@echo "DONE"

$(PROJECT).pdf: $(SOURCES)
	@echo "Generating $@ ... "
	@$(DBLATEX) index.xml -o $@
	@echo "DONE"

$(OVERFILE): $(SRCINDEX)
	@$(XP) $(OVERXSL) $(SRCINDEX) > $@

$(SRCINDEX):
	@echo "<!DOCTYPE includefile ["				> $@
	@for SOURCE in $(TESTS); do \
		echo "<!ENTITY $$SOURCE SYSTEM '$$SOURCE'>"	>> $@; \
	done
	@echo "]> <inc>"					>> $@
	@for SOURCE in $(TESTS); do \
		echo "&$$SOURCE;"				>> $@; \
	done
	@echo "</inc>"						>> $@

$(RESULTFILE): to-docbook
	@cp $(HEADERFILE) $(RESULTFILE)
	@for DOC in `ls $(DOCDIR)/TEST-*.$(EXT)`; do \
		echo "<xi:include href='$$DOC' xmlns:xi='http://www.w3.org/2003/XInclude'/>" \
			>> $(RESULTFILE); \
	done
	@echo "</chapter>" >> $(RESULTFILE)

to-docbook:
	@mkdir -p $(DOCDIR)
	@for SOURCE in $(TESTS); do \
		$(XP) $(TRANSXSL) $(SRCDIR)/$$SOURCE > $(DOCDIR)/$$SOURCE.$(EXT); \
	done

$(ENTITYFILE):
	@echo '<!ENTITY project "$(PROJECT)">'           >> $@
	@echo '<!ENTITY email "$(EMAIL)">'               >> $@
	@echo '<!ENTITY time "$(TIME)">'                 >> $@
	@echo '<!ENTITY hostname "$(HOSTNAME)">'         >> $@
	@echo '<!ENTITY os "$(OS)">'                     >> $@
	@echo '<!ENTITY kernel_name "$(KERNEL_NAME)">'   >> $@
	@echo '<!ENTITY kernel_release "$(KERNEL_REL)">' >> $@
	@echo '<!ENTITY kernel_version "$(KERNEL_VER)">' >> $@

clean:
	@rm -f $(PROJECT).*
	@rm -f $(RESULTFILE)
	@rm -f $(OVERFILE)
	@rm -f $(ENTITYFILE)
	@rm -rf $(DOCDIR)
	@rm -rf $(SRCINDEX)

.PHONY: all $(PROJECT).html clean $(ENTITYFILE) $(SRCINDEX)
