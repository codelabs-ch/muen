include ../../Makeconf

all: $(COMPONENT)

tests: test_$(COMPONENT)
	@obj/tests/test_runner

$(COMPONENT): $(COMPONENT_DEPS)
	@gprbuild $(BUILD_OPTS) -P$@

test_$(COMPONENT): $(TEST_DEPS)
	@gprbuild $(BUILD_OPTS) -P$@ -XBUILD=tests

clean:
	@rm -rf bin obj $(ADDITIONAL_CLEAN)
