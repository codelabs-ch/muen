include ../../Makeconf

all: packer

packer:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

tests: packer_tests
	@$(OBJ_DIR)/test_runner

packer_tests:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

clean:
	@rm -rf $(OBJ_DIR)
