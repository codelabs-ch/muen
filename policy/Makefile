POLICY  = systems/example_system.xml
INC_DIR = include
PAC_DIR = pack

all: process_policy

include ../Makeconf
include ../Makecov

$(INC_DIR):
	@mkdir $@

$(PAC_DIR):
	@mkdir $@

skp_tool:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

process_policy: skp_tool $(POLICY) $(INC_DIR) $(PAC_DIR)
	obj/skpolicy $(POLICY)

tests: build_tests
	@$(OBJ_DIR)/test_runner

build_tests:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -Pskp_tests

build_cov:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) \
		-Pskp_tests -XBUILD="coverage"

clean:
	@rm -rf $(OBJ_DIR) $(INC_DIR) $(PAC_DIR)
