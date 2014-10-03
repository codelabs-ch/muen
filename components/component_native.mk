include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: $(COMPONENT)

$(COMPONENT):
	gprbuild $(BUILD_OPTS) -P$(COMPONENT)

install: $(COMPONENT)
	$(TO_RAW_CMD) $(OBJ_DIR)/$(COMPONENT) $(POLICY_OBJ_DIR)/$(COMPONENT)

clean:
	@rm -rf $(OBJ_DIR)
