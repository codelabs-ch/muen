include ../common_ada.mk

install: $(COMPONENT)
	$(TO_RAW_CMD) $(OBJ_DIR)/$(COMPONENT) $(POLICY_OBJ_DIR)/$(COMPONENT)

clean:
	@rm -rf $(OBJ_DIR)
