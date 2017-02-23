include ../common_ada.mk
include ../cspecs.mk

install: $(COMPONENT) $(INSTALL_TARGETS)
	$(TO_RAW_CMD) $(OBJ_DIR)/$(COMPONENT) $(POLICY_OBJ_DIR)/$(COMPONENT)

clean:
	@rm -rf $(OBJ_DIR)
	@rm -rf $(GEN_DIR)
