include ../cspecs.mk

$(GEN_DIR)/$(COMPONENT).xml: spec/$(COMPONENT).xml $(GEN_DIR) $(MUCGENSPEC) $(CSPEC_TARGETS)
	@$(E) $(COMPONENT) "Generate cspecs" \
		"$(MUCGENSPEC) -i $< -o $@ -I $(GEN_DIR) $(GEN_DIR)"
