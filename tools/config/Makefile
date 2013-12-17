all: skc_tool

include ../../Makeconf
include ../../Makecov

skc_tool:
	@gprbuild $(BUILD_OPTS) -P$@

tests: build_tests
	@$(OBJ_DIR)/test_runner

build_tests:
	@gprbuild $(BUILD_OPTS) -Pskc_tests

build_cov:
	@gprbuild $(BUILD_OPTS) -Pskc_tests -XBUILD="coverage"

clean:
	@rm -rf $(OBJ_DIR)
