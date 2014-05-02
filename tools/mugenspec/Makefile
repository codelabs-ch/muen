COMPONENT = mugenspec

DEPENDS = libmutools

TEMPLATES = src/string_templates.ads
TMPL_SRCS = $(wildcard templates/*)

COMPONENT_TARGETS = $(TEMPLATES)
TEST_TARGETS      = $(COMPONENT_TARGETS)
COV_TARGETS       = $(COMPONENT_TARGETS)

ADDITIONAL_CLEAN = $(TEMPLATES)

include ../component.mk
include ../../Makecov

$(TEMPLATES): $(TMPL_SRCS)
	@echo -n > $@
	@echo -n '--  Auto-generated, '       >> $@
	@date --iso=seconds                   >> $@
	@echo 'package String_Templates is'   >> $@
	@echo '   pragma Style_Checks (Off);' >> $@
	@for tmpl in $(TMPL_SRCS); do \
		echo                                                       >> $@ && \
		echo -n $$tmpl | sed 's/templates\///g;s/\(\-\|\.\)/\_/g;' >> $@ && \
		echo ' : constant String := ""'                            >> $@ && \
		sed -e 's/"/""/g;s/^\(.*\)$$/\& "\1" \& ASCII.LF/g' $$tmpl >> $@ && \
		echo '& "";'                                               >> $@; \
	done
	@echo 'end String_Templates;' >> $@
