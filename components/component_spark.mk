COMP_BIN = $(OBJ_DIR)/debug/$(COMPONENT)

all: $(ALL)

include ../../Makeconf
include ../../Makespark
include ../cspecs.mk
include ../proofs.mk

$(POLICY_OBJ_DIR)/$(COMPONENT): $(COMP_BIN) $(INSTALL_TARGETS)
	 @$(E) $(COMPONENT) Install "$(TO_RAW_CMD) $< $@"

STACK_SIZE = $(COMPONENT_STACK_SIZE)

all: $(STACK_CHECK)

SPARK_OPTS += $(PROOF_OPTS)

$(OBJ_DIR)/%/$(COMPONENT): $(COMPONENT_TARGETS) FORCE
	@$(E) $(COMPONENT) "Build ($*)" \
		"gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=$* -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)"

$(OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)

prepare: $(COMPONENT_TARGETS)

clean:
	@rm -rf $(OBJ_DIR)
	@rm -rf $(GEN_DIR)

FORCE:

.PHONY: FORCE
