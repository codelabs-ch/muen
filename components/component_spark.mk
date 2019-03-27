COMP_BIN = $(OBJ_DIR)/debug/$(COMPONENT)

all: $(ALL)

include ../../Makeconf

COMPLETE_PROOFS := $(shell expr $(GNATPROVE_DATE) \>= 20140901)
ifeq ($(COMPLETE_PROOFS),1)
PROOF_OPTS = -Xproofs=complete
else
PROOF_OPTS = -Xproofs=limited
endif

include ../../Makespark
include ../cspecs.mk
include ../common_components.mk

STACK_SIZE = $(COMPONENT_STACK_SIZE)

all: $(STACK_CHECK)

SPARK_OPTS += $(PROOF_OPTS)

$(OBJ_DIR)/%/$(COMPONENT): $(COMPONENT_TARGETS) FORCE
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=$* -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)

$(OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)

prepare: $(COMPONENT_TARGETS)

FORCE:

.PHONY: FORCE
