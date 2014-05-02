include ../../Makeconf

TEST_RUNNER = @obj/tests/test_runner $(TEST_OPTS)
TOOLS_DIR   = $(TOP_DIR)/tools

all: $(COMPONENT)

$(DEPENDS):
	@$(MAKE) -s -C $(TOOLS_DIR)/$@

tests: test_$(COMPONENT)
	$(TEST_RUNNER)

$(COMPONENT): $(DEPENDS) $(COMPONENT_DEPS)
	@gprbuild $(BUILD_OPTS) -P$@

test_$(COMPONENT): $(TEST_DEPS)
	@gprbuild $(BUILD_OPTS) -P$@ -XBUILD=tests

build_cov: $(COV_DEPS)
	@gprbuild $(BUILD_OPTS) -Ptest_$(COMPONENT) -XBUILD=coverage

clean:
	@rm -rf bin obj $(ADDITIONAL_CLEAN)
