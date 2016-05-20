TMPL_SRCS = $(wildcard templates/*)

ADDITIONAL_CLEAN += $(TEMPLATES)

$(TEMPLATES): $(TMPL_SRCS)
	@mkdir -p `dirname $(TEMPLATES)`
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
