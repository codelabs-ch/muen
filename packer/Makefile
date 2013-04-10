KERN_DIR = $(CURDIR)/../kernel

include ../Makeconf

all: $(OBJ_DIR)/kernel

packer:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

$(OBJ_DIR)/kernel: packer
	$(SKPACKER) $(KERN_DIR)/obj/kernel $@

tests: build_tests
	@$(OBJ_DIR)/test_runner

build_tests:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -Ppacker_tests

clean:
	@rm -rf $(OBJ_DIR)
