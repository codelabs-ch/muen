NUM_CPUS := $(shell getconf _NPROCESSORS_ONLN)

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
	mugenspec      \
	mugenukvm      \
	mugenvtd       \
	mugenzp        \
	mulnxbzpatch   \
	mupack         \
	mupcspkrdbg    \
	muwalkpt

# Projects to test
TESTS =      \
	$(LIBS)  \
	$(filter-out mugenschedcfg,$(TOOLS))

# Projects to clean
CLEAN =              \
	$(LIBS)          \
	$(LIBS_NO_TESTS) \
	$(TOOLS)

# Projects to prepare
PREPARE =   \
	$(LIBS) \
	$(TOOLS)

TOOLS_PREPARE = $(PREPARE:%=prepare-%)
TOOLS_INSTALL = $(TOOLS:%=install-%)
TOOLS_CLEAN   = $(CLEAN:%=clean-%)

all: build_tools

build_tools: prepare
	gprbuild $(BUILD_OPTS) -P$@

tests:
	@for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

install: $(TOOLS_INSTALL)
$(TOOLS_INSTALL):
	$(MAKE) -C $(@:install-%=%) install

clean: $(TOOLS_CLEAN)
$(TOOLS_CLEAN):
	$(MAKE) -C $(@:clean-%=%) clean

prepare: $(TOOLS_PREPARE)
$(TOOLS_PREPARE):
	$(MAKE) -C $(@:prepare-%=%) prepare
