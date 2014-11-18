include ../Makeconf

COMPONENTS ?=    \
	libmuchannel \
	linux        \
	sm           \
	tau0         \
	time         \
	vt

COMPONENTS_INSTALL = $(COMPONENTS:%=install-%)
COMPONENTS_CLEAN   = $(COMPONENTS:%=clean-%)

all: $(COMPONENTS) install

$(COMPONENTS):
	@$(MAKE) -C $@

install: $(COMPONENTS_INSTALL)
$(COMPONENTS_INSTALL): $(COMPONENTS)
	@$(MAKE) -C $(@:install-%=%) install

clean: $(COMPONENTS_CLEAN)
$(COMPONENTS_CLEAN):
	@$(MAKE) -C $(@:clean-%=%) clean

.PHONY: $(COMPONENTS)
