include ../../Makeconf

all: $(COMPONENT)

prepare: $(COMPONENT_TARGETS)

$(COMPONENT): prepare
	@$(E) $(COMPONENT) Build "gprbuild $(BUILD_OPTS) -P$@ $(EXTRA_BUILD_OPTS)"

install: $(COMPONENT)
	@$(E) $(COMPONENT) Install \
		"install -m 755 -D bin/$(COMPONENT) $(PREFIX)/bin/$(COMPONENT)"

clean:
	@rm -rf bin obj $(ADDITIONAL_CLEAN)
