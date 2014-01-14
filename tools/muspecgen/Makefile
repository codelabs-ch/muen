TOOL = muspecgen

TEMPLATES = src/string_templates.ads
TMPL_SRCS = $(wildcard templates/*)

all: $(TOOL)

include ../../Makeconf
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

$(TOOL): $(TEMPLATES)
	@gprbuild $(BUILD_OPTS) -P$@

tests: test_$(TOOL)
	@obj/tests/test_runner

test_$(TOOL): $(TEMPLATES)
	@gprbuild $(BUILD_OPTS) -P$@ -XBUILD=tests

build_cov: $(TEMPLATES)
	@gprbuild $(BUILD_OPTS) -Ptest_$(TOOL) -XBUILD=coverage

clean:
	@rm -rf bin obj
	@rm -f $(TEMPLATES)
