$(GEN_DIR)/.cspecs:
	$(MUCGENSPEC) -p $(POLICY_SRC) -c $(COMPONENT) $(GEN_DIR)
	@touch $@
