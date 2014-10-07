include ../../Makeconf
include ../../Makespark

all: $(ALL) install

$(OBJ_DIR)/debug/$(COMPONENT):
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=debug

$(OBJ_DIR)/release/$(COMPONENT):
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=release

$(OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)
	@cp $< $@

install: $(OBJ_DIR)/$(COMPONENT)
	$(TO_RAW_CMD) $< $(POLICY_OBJ_DIR)/$(COMPONENT)

clean:
	@rm -rf $(OBJ_DIR)

FORCE:

.PHONY: FORCE $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)
