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

all: $(COMPONENTS) tau0 install

$(COMPONENTS) tau0:
	@$(MAKE) -C $@

install: $(COMPONENTS_INSTALL) install-tau0
$(COMPONENTS_INSTALL): $(COMPONENTS)
	@$(MAKE) -C $(@:install-%=%) install

install-tau0:
	@$(MAKE) -C $(@:install-%=%) install

tests:
	@for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

clean: $(COMPONENTS_CLEAN) clean-tau0
$(COMPONENTS_CLEAN) clean-tau0:
	@$(MAKE) -C $(@:clean-%=%) clean

.NOTPARALLEL:

.PHONY: $(COMPONENTS) tau0
