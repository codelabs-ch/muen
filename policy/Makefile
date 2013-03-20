include ../Makeconf

all:

tests: build_tests
	@$(OBJ_DIR)/test_runner

build_tests:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -Pskp_tests

clean:
	@rm -rf $(OBJ_DIR)

.PHONY: tests
