include ../cspecs.mk


$(GEN_DIR)/$(COMPONENT).xml: spec/$(COMPONENT).xml $(CSPEC_TARGETS) $(MUCGENSPEC) $(MUXMLFILTER) | $(GEN_DIR)
	@$(E) $(COMPONENT) "Generate xml cspecs" \
		"$(MUCFGCVRESALLOC) $< $@ -I $(GEN_DIR)"
	@$(E) $(COMPONENT) "Filter xml cspecs" \
		"$(MUXMLFILTER) -isn Component_Ext -osn Component $@ $(GEN_DIR)/$(COMPONENT)_filtered.xml"
	@$(E) $(COMPONENT) "Generate cspecs" \
		"$(MUCGENSPEC) -i $(GEN_DIR)/$(COMPONENT)_filtered.xml $(GEN_DIR)"
