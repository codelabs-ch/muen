# Tool dependencies and tool projects
TOOLS =        \
	libmuxml   \
	libmutools \
	muacpigen  \
	muiobmgen  \
	mumsrbmgen \
	mupack     \
	muptgen    \
	muspecgen  \
	muzpgen

# Implicitly built libraries
LIBS =       \
	libmugen \
	libmulog \
	libtest

# Projects to clean
CLEAN =      \
	$(TOOLS) \
	$(LIBS)  \
	config   \
	packer   \
	policy   \

all: skconfig skpacker skpolicy tools

skconfig:
	$(MAKE) -C config

skpacker:
	$(MAKE) -C packer

skpolicy:
	$(MAKE) -C policy

tools:
	@for prj in $(TOOLS); do $(MAKE) -C $$prj || exit 1; done

tests:
	@for prj in $(TOOLS); do $(MAKE) $@ -C $$prj || exit 1; done

clean:
	@for prj in $(CLEAN); do $(MAKE) $@ -C $$prj || exit 1; done
