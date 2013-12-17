include ../../Makeconf

all: packer

packer:
	@gprbuild $(BUILD_OPTS) -P$@

tests: packer_tests
	@$(OBJ_DIR)/test_runner

packer_tests:
	@gprbuild $(BUILD_OPTS) -P$@

clean:
	@rm -rf $(OBJ_DIR)
