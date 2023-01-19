COMPONENT = mucbinsplit

TEST_TARGETS = $(OBJ_DIR)/text.ref $(OBJ_DIR)/data.ref $(OBJ_DIR)/rodata.ref

include ../component.mk
include ../coverage.mk
include ../../build-cfg/mk/tests.mk

$(OBJ_DIR)/%.ref: data/test_binary $(OBJ_DIR)
	objcopy -Obinary --only-section=.$* $< $@

$(OBJ_DIR):
	mkdir -p $@

