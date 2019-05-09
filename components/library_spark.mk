include ../common.mk

$(POLICY_OBJ_DIR)/$(COMPONENT): $(INSTALL_TARGETS)

clean:
	@rm -rf $(OBJ_DIR) $(GEN_DIR) lib
