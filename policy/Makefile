POLICY = data/example_system.xml

include ../Makeconf

all: process_policy

skp_tool:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

process_policy: skp_tool $(POLICY)
	obj/skpolicy $(POLICY)

tests: build_tests
	@$(OBJ_DIR)/test_runner

build_tests:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -Pskp_tests

clean:
	@rm -rf $(OBJ_DIR) include/skp-*.ads include/*.pt

.PHONY: tests
