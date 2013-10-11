all: skp_tools

include ../../Makeconf
include ../../Makecov

skp_tools:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

tests: skp_tests
	@$(OBJ_DIR)/test_runner

skp_tests:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

build_cov:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) \
		-Pskp_tests -XBUILD="coverage"

clean:
	@rm -rf $(OBJ_DIR)
