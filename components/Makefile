include ../Makeconf

COMPONENTS ?=    \
	libmuchannel \
	libmutime    \
	libdebuglog  \
	libmusinfo   \
	crypter      \
	dbgserver    \
	idle         \
	linux        \
	sl           \
	sm           \
	tau0         \
	time         \
	vt

TESTS = libmutime libmusinfo

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
