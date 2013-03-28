POLICY  = data/example_system.xml
OUT_DIR = include

include ../Makeconf

all: process_policy

$(OUT_DIR):
	@mkdir $@

skp_tool:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

process_policy: skp_tool $(POLICY) $(OUT_DIR)
	obj/skpolicy $(POLICY)

tests: build_tests
	@$(OBJ_DIR)/test_runner

build_tests:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -Pskp_tests

clean:
	@rm -rf $(OBJ_DIR) $(OUT_DIR)

.PHONY: tests
