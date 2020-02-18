include ../Makeconf

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
TOOLS =            \
	mucbinsplit    \
	mucfgalloc     \
	mucfgcjoin     \
	mucfgexpand    \
	mucfgmemhashes \
	mucfgmerge     \
	mucfgvalidate  \
	mucgenspec     \
	mucheckelf     \
	mucheckstack   \
	mugenacpi      \
	mugeniobm      \
	mugenmsrbm     \
	mugenmsrstore  \
	mugenpt        \
	mugenschedcfg  \
	mugensinfo     \
	mugensolo5     \
	mugenspec      \
	mugentau0cmds  \
	mugenvtd       \
	mugenzp        \
	mulnxbzpatch   \
	mupack         \
	mupcspkrdbg    \
	muwalkpt \
	sbs

# Projects to test
TESTS := $(LIBS) $(TOOLS)
TESTS := $(filter-out mugenschedcfg,$(TESTS))
TESTS := $(filter-out sbs,$(TESTS))

# Projects to clean
CLEAN =              \
	$(LIBS)          \
	$(LIBS_NO_TESTS) \
	$(TOOLS)

# Projects to prepare
PREPARE := $(LIBS) $(TOOLS)

TOOLS_PREPARE = $(PREPARE:%=prepare-%)
TOOLS_INSTALL = $(TOOLS:%=install-%)
TOOLS_CLEAN   = $(CLEAN:%=clean-%)

LOG = $(OBJ_DIR)/tools.log

all: build_tools

build_tools: prepare
	@$(E) tools Build "gprbuild $(BUILD_OPTS) -P$@" $(LOG)

tests:
	for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

install: $(TOOLS_INSTALL)
$(TOOLS_INSTALL):
	$(MAKE) -C $(@:install-%=%) install

clean: $(TOOLS_CLEAN)
	rm -rf $(OBJ_DIR)
$(TOOLS_CLEAN):
	$(MAKE) -C $(@:clean-%=%) clean

prepare: $(TOOLS_PREPARE)
$(TOOLS_PREPARE):
	$(MAKE) -C $(@:prepare-%=%) prepare
