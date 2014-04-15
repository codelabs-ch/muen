PACKAGE = kernel

VERSION_SPEC = src/sk-version.ads
VERSION      = $(GIT_REV)
GIT_REV      = $(shell git describe --always --dirty="-UNCLEAN")

include ../Makeconf
include ../Makespark

all: $(ALL) install

SLOC_DIRS = $(SRC_DIR) $(TOP_DIR)/common/src $(TOP_DIR)/rts/src

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

$(OBJ_DIR)/debug/$(PACKAGE): $(VERSION_SPEC)
	gprbuild $(BUILD_OPTS) -P$(PACKAGE) -Xbuild=debug

$(OBJ_DIR)/release/$(PACKAGE): $(VERSION_SPEC)
	gprbuild $(BUILD_OPTS) -P$(PACKAGE) -Xbuild=release

$(OBJ_DIR)/$(PACKAGE): $(OBJ_DIR)/debug/$(PACKAGE) $(OBJ_DIR)/release/$(PACKAGE)
	@cp $< $@

install: $(OBJ_DIR)/$(PACKAGE)
	@$(TO_RAW_CMD) $^ $(POLICY_DIR)/$(PACKAGE)

clean:
	rm -rf $(OBJ_DIR)
	rm -f .git-rev

sloc:
	find $(SLOC_DIRS) -type f -exec sloccount {} \+

FORCE:

.PHONY: FORCE
