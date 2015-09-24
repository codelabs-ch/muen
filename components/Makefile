include ../Makeconf

COMPONENTS ?=    \
	libmuchannel \
	libdebuglog  \
	crypter      \
	dbgserver    \
	idle         \
	linux        \
	sm           \
	tau0         \
	time         \
	vt

TESTS = libmutime

COMPONENTS_INSTALL = $(COMPONENTS:%=install-%)
COMPONENTS_CLEAN   = $(COMPONENTS:%=clean-%)

all: $(COMPONENTS) install

$(COMPONENTS):
	@$(MAKE) -C $@

install: $(COMPONENTS_INSTALL)
$(COMPONENTS_INSTALL): $(COMPONENTS)
	@$(MAKE) -C $(@:install-%=%) install

tests:
	@for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

clean: $(COMPONENTS_CLEAN)
$(COMPONENTS_CLEAN):
	@$(MAKE) -C $(@:clean-%=%) clean

.NOTPARALLEL:

.PHONY: $(COMPONENTS)
