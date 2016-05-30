COMPONENT = kernel

VERSION_SPEC = src/sk-version.ads
VERSION      = $(GIT_REV)
GIT_REV      = $(shell git describe --always --dirty="-UNCLEAN")

GNATPROVE_DATE  := $(shell gnatprove --version | cut -d "(" -f2 | cut -d ")" -f1)
COMPLETE_PROOFS := $(shell expr $(GNATPROVE_DATE) \>= 20141106)
ifeq ($(COMPLETE_PROOFS),1)
PROOF_OPTS = -Xproofs=complete
else
PROOF_OPTS = -Xproofs=limited
endif

include ../Makeconf
include ../Makespark

SPARK_OPTS += $(PROOF_OPTS)

all: $(ALL) install

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

$(OBJ_DIR)/debug/$(COMPONENT): $(VERSION_SPEC)
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=debug $(PROOF_OPTS)

$(OBJ_DIR)/release/$(COMPONENT): $(VERSION_SPEC)
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xbuild=release $(PROOF_OPTS)

$(OBJ_DIR)/$(COMPONENT): $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)
	@cp $< $@

install: $(OBJ_DIR)/$(COMPONENT)
	@$(MUCHECKELF) $(POLICY_B) $^
	@$(TO_RAW_CMD) $^ $(POLICY_OBJ_DIR)/$(COMPONENT)

clean:
	rm -rf $(OBJ_DIR)
	rm -f .git-rev

sloc:
	find $(SLOC_DIRS) -type f -exec sloccount {} \+

FORCE:

.PHONY: FORCE $(OBJ_DIR)/debug/$(COMPONENT) $(OBJ_DIR)/release/$(COMPONENT)
