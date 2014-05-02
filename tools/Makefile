# Library projects
LIBS =            \
	libmuxml      \
	libmutools    \
	libmucfgcheck \
	libmucfgvcpu  \
	libpaging

# Implicitly built libraries
IMP_LIBS =   \
	libmulog \
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
	mugenpt       \
	mugenspec     \
	mugenzp       \
	mulnxbzpatch  \
	mupack

# Projects to test
TESTS =      \
	$(LIBS)  \
	$(TOOLS)

# Projects to clean
CLEAN =         \
	$(LIBS)     \
	$(IMP_LIBS) \
	$(TOOLS)

all: tools

tools:
	@for prj in $(TOOLS); do $(MAKE) -C $$prj || exit 1; done

tests:
	@for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

clean:
	@for prj in $(CLEAN); do $(MAKE) $@ -C $$prj || exit 1; done
