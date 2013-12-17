all: skp_tools

include ../../Makeconf
include ../../Makecov

skp_tools:
	@gprbuild $(BUILD_OPTS) -P$@

tests: skp_tests
	@$(OBJ_DIR)/test_runner

skp_tests:
	@gprbuild $(BUILD_OPTS) -P$@

build_cov:
	@gprbuild $(BUILD_OPTS) -Pskp_tests -XBUILD="coverage"

clean:
	@rm -rf $(OBJ_DIR)
