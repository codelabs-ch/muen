COMPONENT = kernel

VERSION_SPEC = src/sk-version.ads
VERSION      = $(GIT_REV)
GIT_REV      = $(shell git describe --always --dirty="-UNCLEAN")

include ../Makeconf

COMPLETE_PROOFS := $(shell expr $(GNATPROVE_DATE) \>= 20141106)
ifeq ($(COMPLETE_PROOFS),1)
PROOF_OPTS = -Xproofs=complete
else
PROOF_OPTS = -Xproofs=limited
endif

PROOF_TARGETS += $(VERSION_SPEC)

include ../Makespark

SPARK_OPTS += $(PROOF_OPTS)

AXIOM_GUARD := $(shell expr $(GNATPROVE_DATE) \>= 20170313)
ifeq ($(AXIOM_GUARD),1)
SPARK_OPTS += --no-axiom-guard
endif

STACK_SIZE = $(KERNEL_STACK_SIZE)

ELF_CHECK = $(OBJ_DIR)/.elfcheck_ok

all: $(STACK_CHECK) $(POLICY_OBJ_DIR)/$(COMPONENT)

SLOC_DIRS = $(SRC_DIR) features $(TOP_DIR)/common/src $(TOP_DIR)/rts/src

.git-rev: FORCE
	@if [ -r $@ ]; then \
		if [ "$$(cat $@)" != "$(GIT_REV)" ]; then \
			echo $(GIT_REV) > $@; \
		fi; \
	else \
		echo $(GIT_REV) > $@; \
	fi

$(VERSION_SPEC): .git-rev
	@echo "package SK.Version is"                   > $@
	@echo "   Version_String : constant String :=" >> $@
	@echo "     \"$(VERSION)\";"                   >> $@
	@echo "end SK.Version;"                        >> $@

$(OBJ_DIR)/%/$(COMPONENT): $(VERSION_SPEC) FORCE
	@$(E) $(COMPONENT) "Build ($*)" \
		"gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=$* -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)"

$(OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)

$(ELF_CHECK): $(OBJ_DIR)/debug/$(COMPONENT) $(MUCHECKELF)
	@$(E) $(COMPONENT) "Check ELF" "$(MUCHECKELF) $(POLICY_B) $<"
	@touch $@

$(POLICY_OBJ_DIR)/$(COMPONENT): $(ELF_CHECK)
	@$(E) $(COMPONENT) Install "$(TO_RAW_CMD) $(OBJ_DIR)/debug/$(COMPONENT) $@"

clean:
	rm -rf $(OBJ_DIR)
	rm -f .git-rev

sloc:
	find $(SLOC_DIRS) -type f -exec sloccount {} \+

metrics:
	mkdir -p $(OBJ_DIR)/release
	gnatmetric -q -a -P$(COMPONENT) -Xbuild=release

FORCE:

.PHONY: FORCE
