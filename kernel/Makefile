PACKAGE = kernel

VERSION_SPEC = src/sk-version.ads
VERSION      = $(GIT_REV)
GIT_REV      = $(shell git describe --always --dirty="-UNCLEAN")

include ../Makeconf
include ../Makespark

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

clean:
	rm -rf $(OBJ_DIR)
	rm -f .git-rev

FORCE:

.PHONY: FORCE
