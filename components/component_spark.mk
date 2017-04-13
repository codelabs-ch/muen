GNATPROVE_DATE  := $(shell gnatprove --version | cut -d "(" -f2 | cut -d ")" -f1)
COMPLETE_PROOFS := $(shell expr $(GNATPROVE_DATE) \>= 20140901)
ifeq ($(COMPLETE_PROOFS),1)
PROOF_OPTS = -Xproofs=complete
else
PROOF_OPTS = -Xproofs=limited
endif

all: $(ALL)

include ../../Makeconf
include ../../Makespark
include ../cspecs.mk

STACK_SIZE = $(COMPONENT_STACK_SIZE)

all: $(STACK_CHECK)

SPARK_OPTS += $(PROOF_OPTS)

$(OBJ_DIR)/%/$(COMPONENT): $(COMPONENT_TARGETS) FORCE
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=$* -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)

$(OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)

$(POLICY_OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(INSTALL_TARGETS)
	$(TO_RAW_CMD) $< $@

clean:
	@rm -rf $(OBJ_DIR) $(GEN_DIR)

FORCE:

.NOTPARALLEL:

.PHONY: FORCE
