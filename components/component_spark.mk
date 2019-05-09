include ../common.mk

$(POLICY_OBJ_DIR)/$(COMPONENT): $(COMP_BIN) $(INSTALL_TARGETS)
	 @$(E) $(COMPONENT) Install "$(TO_RAW_CMD) $< $@"

clean:
	@rm -rf $(OBJ_DIR) $(GEN_DIR)
