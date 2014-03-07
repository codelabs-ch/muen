include ../../Makeconf

all: $(TOOL)

tests: test_$(TOOL)
	@obj/tests/test_runner

$(TOOL): $(TOOL_DEPS)
	@gprbuild $(BUILD_OPTS) -P$@

test_$(TOOL): $(TEST_DEPS)
	@gprbuild $(BUILD_OPTS) -P$@ -XBUILD=tests

clean:
	@rm -rf bin obj $(ADDITIONAL_CLEAN)
