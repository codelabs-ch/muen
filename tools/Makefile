# Tool dependencies and tool projects
TOOLS =           \
	libmuxml      \
	libmutools    \
	libmucfgcheck \
	libpaging     \
	mucfgalloc    \
	mucfgexpand   \
	mucfgvalidate \
	mugenacpi     \
	mugeniobm     \
	mugenmsrbm    \
	mugenpt       \
	mugenspec     \
	mugenzp       \
	mulnxbzpatch  \
	mupack

# Implicitly built libraries
LIBS =       \
	libmulog \
	libtest

# Projects to clean
CLEAN =      \
	$(TOOLS) \
	$(LIBS)

all: tools

tools:
	@for prj in $(TOOLS); do $(MAKE) -C $$prj || exit 1; done

tests:
	@for prj in $(TOOLS); do $(MAKE) $@ -C $$prj || exit 1; done

clean:
	@for prj in $(CLEAN); do $(MAKE) $@ -C $$prj || exit 1; done
