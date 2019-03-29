COMP_BIN = $(OBJ_DIR)/debug/$(COMPONENT)

all: $(ALL)

include ../../Makeconf
include ../../Makespark
include ../cspecs.mk
include ../common_components.mk
include ../proofs.mk

STACK_SIZE = $(COMPONENT_STACK_SIZE)

all: $(STACK_CHECK)

SPARK_OPTS += $(PROOF_OPTS)

$(OBJ_DIR)/%/$(COMPONENT): $(COMPONENT_TARGETS) FORCE
	@$(E) $(COMPONENT) "Build ($*)" \
		"gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=$* -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)"

$(OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)

prepare: $(COMPONENT_TARGETS)

FORCE:

.PHONY: FORCE
