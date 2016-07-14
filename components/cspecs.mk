cspecs: $(GEN_DIR)/.cspecs
$(GEN_DIR)/.cspecs: $(MUCGENSPEC) $(POLICY_SRC)
	$(MUCGENSPEC) -p $(POLICY_SRC) -c $(COMPONENT) $(GEN_DIR)
	@touch $@
