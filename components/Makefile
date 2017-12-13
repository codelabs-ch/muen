include ../Makeconf

COMPONENTS ?=    \
	libmuchannel \
	libmudm      \
	libmutime    \
	libdebuglog  \
	libmusinfo   \
	libxhcidbg   \
	crypter      \
	dbgserver    \
	dm           \
	idle         \
	linux        \
	ps2_drv      \
	sl           \
	sm           \
	time         \
	vt

TESTS = libmutime libmusinfo

INST_TARGET = $(POLICY_OBJ_DIR)/$(@:install-%=%)

COMPONENTS_INSTALL = $(COMPONENTS:%=install-%)
COMPONENTS_CLEAN   = $(COMPONENTS:%=clean-%)

all: $(COMPONENTS) install

$(COMPONENTS) tau0:
	@$(MAKE) -C $@

install: $(COMPONENTS_INSTALL)
$(COMPONENTS_INSTALL): $(COMPONENTS)
	@$(MAKE) -C $(@:install-%=%) $(INST_TARGET)

install-tau0:
	@$(MAKE) -C $(@:install-%=%) $(INST_TARGET)

tests:
	@for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

clean: $(COMPONENTS_CLEAN) clean-tau0
$(COMPONENTS_CLEAN) clean-tau0:
	@$(MAKE) -C $(@:clean-%=%) clean

.NOTPARALLEL:

.PHONY: $(COMPONENTS) tau0
