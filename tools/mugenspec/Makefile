COMPONENT = mugenspec

TEMPLATES = src/string_templates.ads
TMPL_SRCS = $(wildcard templates/*)

COMPONENT_DEPS = $(TEMPLATES)
TEST_DEPS      = $(COMPONENT_DEPS)

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

build_cov: $(TEMPLATES)
	@gprbuild $(BUILD_OPTS) -Ptest_$(COMPONENT) -XBUILD=coverage
