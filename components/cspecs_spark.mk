include ../cspecs.mk

$(GEN_DIR)/$(COMPONENT).xml: spec/$(COMPONENT).xml $(MUCGENSPEC) $(CSPEC_TARGETS) | $(GEN_DIR)
	@$(E) $(COMPONENT) "Generate cspecs" \
		"$(MUCFGCVAALLOC) $< $@ -I $(GEN_DIR) && $(MUCGENSPEC) -i $@ $(GEN_DIR)"
