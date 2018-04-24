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
	mucheckpt      \
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
	mupcspkrdbg

# Projects to build
PROJECTS =   \
	$(LIBS)  \
	$(TOOLS) \

# Projects to test
TESTS =      \
	$(LIBS)  \
	$(filter-out mugenschedcfg,$(TOOLS))

# Projects to clean
CLEAN =              \
	$(LIBS)          \
	$(LIBS_NO_TESTS) \
	$(TOOLS)

all: projects

projects:
	@for prj in $(PROJECTS); do $(MAKE) -C $$prj || exit 1; done

tests:
	@for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

install:
	@for prj in $(TOOLS); do $(MAKE) $@ -C $$prj PREFIX=$(PREFIX) || exit 1; done

clean:
	@for prj in $(CLEAN); do $(MAKE) $@ -C $$prj || exit 1; done
