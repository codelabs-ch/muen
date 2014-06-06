# Library projects
LIBS =            \
	libmuxml      \
	libmutools    \
	libmucfgcheck \
	libmucfgvcpu  \
	libmusinfo    \
	liballoc      \
	libpaging

# Library projects with no unit test suite
LIBS_NO_TESTS = \
	libmulog    \
	libtest

# Tool projects
TOOLS =           \
	mucfgalloc    \
	mucfgexpand   \
	mucfgmerge    \
	mucfgvalidate \
	mugenacpi     \
	mugeniobm     \
	mugenmsrbm    \
	mugenmsrstore \
	mugenpt       \
	mugensinfo    \
	mugenspec     \
	mugenzp       \
	mulnxbzpatch  \
	mupack

# Projects to test
TESTS =      \
	$(LIBS)  \
	$(TOOLS)

# Projects to clean
CLEAN =              \
	$(LIBS)          \
	$(LIBS_NO_TESTS) \
	$(TOOLS)

all: tools

tools:
	@for prj in $(TOOLS); do $(MAKE) -C $$prj || exit 1; done

tests:
	@for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

clean:
	@for prj in $(CLEAN); do $(MAKE) $@ -C $$prj || exit 1; done
