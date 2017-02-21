cspecs: $(GEN_DIR)/.cspecs
$(GEN_DIR)/.cspecs: $(MUCGENSPEC) $(GENERATE_CSPECS_FROM)
	$(MUCGENSPEC) -i $(GENERATE_CSPECS_FROM) $(GEN_DIR)
	@touch $@
