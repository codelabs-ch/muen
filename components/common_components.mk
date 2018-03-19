$(POLICY_OBJ_DIR)/$(COMPONENT): $(COMP_BIN) $(INSTALL_TARGETS)
	$(TO_RAW_CMD) $< $@

clean:
	@rm -rf $(OBJ_DIR)
	@rm -rf $(GEN_DIR)
