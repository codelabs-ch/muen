include ../../Makeconf

all: $(COMPONENT)

$(COMPONENT): $(COMPONENT_TARGETS)
	gprbuild $(BUILD_OPTS) -P$@

install: $(COMPONENT)
	install -m 755 -D bin/$(COMPONENT) $(PREFIX)/bin/$(COMPONENT)

clean:
	@rm -rf bin obj $(ADDITIONAL_CLEAN)
