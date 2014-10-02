include ../Makeconf

COMPONENTS = \
	linux    \
	sm       \
	tau0     \
	time     \
	vt

COMPONENTS_CLEAN = $(COMPONENTS:%=clean-%)

all: $(COMPONENTS) install

$(COMPONENTS):
	@$(MAKE) -C $@

install:
	@for component in $(COMPONENTS); do \
		$(MAKE) $@ -C $$component || exit 1; \
	done

clean: $(COMPONENTS_CLEAN)
$(COMPONENTS_CLEAN):
	@$(MAKE) -C $(@:clean-%=%) clean

.PHONY: $(COMPONENTS)
