GNATPROVE_DATE  := $(shell gnatprove --version | cut -d "(" -f2 | cut -d ")" -f1)
COMPLETE_PROOFS := $(shell expr $(GNATPROVE_DATE) \>= 20140901)
ifeq ($(COMPLETE_PROOFS),1)
PROOF_OPTS = -Xproofs=complete
else
PROOF_OPTS = -Xproofs=limited
endif

ifdef GENERATE_CSPECS
ALL += cspecs
endif

all: $(ALL)

include ../../Makeconf
include ../../Makespark
include ../cspecs.mk

SPARK_OPTS += $(PROOF_OPTS)

$(OBJ_DIR)/%/$(COMPONENT): FORCE
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=$* -Xstacksize=$(COMPONENT_STACK_SIZE) $(PROOF_OPTS)
	build=$* $(MUCHECKSTACK) -P$(COMPONENT) -l$(COMPONENT_STACK_SIZE)

$(OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)
	@cp $< $@

install: $(OBJ_DIR)/$(COMPONENT)
	$(TO_RAW_CMD) $< $(POLICY_OBJ_DIR)/$(COMPONENT)

clean:
	@rm -rf $(OBJ_DIR) $(GEN_DIR)

FORCE:

.NOTPARALLEL:

.PHONY: FORCE
