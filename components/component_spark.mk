GNATPROVE_DATE  := $(shell gnatprove --version | cut -d "(" -f2 | cut -d ")" -f1)
COMPLETE_PROOFS := $(shell expr $(GNATPROVE_DATE) \>= 20140901)
ifeq ($(COMPLETE_PROOFS),1)
PROOF_OPTS = -Xproofs=complete
else
PROOF_OPTS = -Xproofs=limited
endif

include ../../Makeconf
include ../../Makespark

SPARK_OPTS += $(PROOF_OPTS)

all: $(ALL) install

$(OBJ_DIR)/debug/$(COMPONENT):
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=debug $(PROOF_OPTS)

$(OBJ_DIR)/release/$(COMPONENT):
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=release $(PROOF_OPTS)

$(OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)
	@cp $< $@

install: $(OBJ_DIR)/$(COMPONENT)
	$(TO_RAW_CMD) $< $(POLICY_OBJ_DIR)/$(COMPONENT)

clean:
	@rm -rf $(OBJ_DIR)

.PHONY: $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)
