include ../../Makeconf

all: $(COMPONENT)

prepare: $(COMPONENT_TARGETS)

$(COMPONENT): prepare
	gprbuild $(BUILD_OPTS) -P$@

install: $(COMPONENT)
	install -m 755 -D bin/$(COMPONENT) $(PREFIX)/bin/$(COMPONENT)

clean:
	@rm -rf bin obj $(ADDITIONAL_CLEAN)
