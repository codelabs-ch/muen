all: skc_tool

include ../Makeconf
include ../Makecov

skc_tool:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

tests: build_tests
	@$(OBJ_DIR)/test_runner

build_tests:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -Pskc_tests

build_cov:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) \
		-Pskc_tests -XBUILD="coverage"

clean:
	@rm -rf $(OBJ_DIR)
