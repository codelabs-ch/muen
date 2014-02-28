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

$(OBJ_DIR)/$(PACKAGE): $(VERSION_SPEC)
	gprbuild $(BUILD_OPTS) -P$(PACKAGE)

install: $(OBJ_DIR)/$(PACKAGE)
	@cp $^ $(POLICY_DIR)

clean:
	rm -rf $(OBJ_DIR)
	rm -f .git-rev

sloc:
	find $(SLOC_DIRS) -type f -exec sloccount {} \+

FORCE:

.PHONY: FORCE
