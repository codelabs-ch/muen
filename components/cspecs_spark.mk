include ../cspecs.mk

$(GEN_DIR)/$(COMPONENT).xml: spec/$(COMPONENT).xml $(MUCGENSPEC) $(CSPEC_TARGETS) | $(GEN_DIR)
	@$(E) $(COMPONENT) "Generate cspecs" \
		"$(MUCGENSPEC) -i $< -o $@ -I $(GEN_DIR) $(GEN_DIR)"
