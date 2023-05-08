include ../build-cfg/mk/Makeconf

# Library projects
LIBS =            \
	libmuxml      \
	libmutools    \
	libmucfgcheck \
	libmucfgvcpu  \
	liballoc      \
	libpaging

# Library projects with no unit test suite
LIBS_NO_TESTS = \
	libmulog    \
	libtest

# Tool projects
TOOLS =             \
	mucbinsplit     \
	mucfgalloc      \
	mucfgcjoin      \
	mucfgcvresalloc \
	mucfgvresalloc  \
	mucfgexpand     \
	mucfgmemhashes  \
	mucfgmerge      \
	mucfgvalidate   \
	mucgenspec      \
	mucheckelf      \
	mucheckstack    \
	mugenacpi       \
	mugenmsrstore   \
	mugenpt         \
	mugensinfo      \
	mugensolo5      \
	mugenspec       \
	mugentau0cmds   \
	mugenzp         \
	mupcspkrdbg     \
	muwalkpt        \
	sbs

# Projects to test
TESTS := $(LIBS) $(TOOLS)
TESTS := $(filter-out sbs,$(TESTS))

MUEN_TOOLS ?=        \
	$(LIBS)          \
	$(LIBS_NO_TESTS) \
	$(TOOLS)

# Projects to prepare
PREPARE := $(MUEN_LIBS) $(MUEN_TOOLS)
INSTALL := $(filter-out lib%,$(MUEN_TOOLS))

TOOLS_PREPARE = $(MUEN_TOOLS:%=.prepare-%)
TOOLS_INSTALL = $(INSTALL:%=install-%)
TOOLS_CLEAN   = $(MUEN_TOOLS:%=clean-%)

LOG = $(OBJ_DIR)/tools.log

all: build_tools

build_tools: $(OBJ_DIR)/build.gpr $(TOOLS_PREPARE)
	@$(E) tools Build "gprbuild $(BUILD_OPTS) -P$<" $(LOG)

tests:
	for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

install: $(TOOLS_INSTALL)
$(TOOLS_INSTALL): build_tools
	$(MAKE) -C $(@:install-%=%) install

clean: $(TOOLS_CLEAN)
	rm -rf $(OBJ_DIR) .prepare-*
$(TOOLS_CLEAN):
	$(MAKE) -C $(@:clean-%=%) clean

prepare: $(TOOLS_PREPARE)
$(TOOLS_PREPARE):
	$(MAKE) -C $(@:.prepare-%=%) prepare
	@touch $@

$(OBJ_DIR):
	mkdir -p $@

$(OBJ_DIR)/build.gpr: | $(OBJ_DIR)
	@echo 'aggregate project Build is'       > $@
	@echo '   for Project_Files use ('                                    >> $@
	@$(foreach c,$(MUEN_TOOLS),echo -n '      "../$(c)/$(c).gpr"'  >> $@; \
		if [ $(c) != $(lastword $(MUEN_TOOLS)) ]; then echo ','    >> $@; \
			else echo '' >> $@; fi;)
	@echo '   );'                                                         >> $@
	@echo 'end Build;'                                                    >> $@

.PHONY: $(OBJ_DIR)/build.gpr
