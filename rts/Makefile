COMPONENT = rts

include ../Makeconf

INCLUDE_SRC  = $(wildcard src/*.ad*)
INCLUDE_SRC += $(wildcard src/asm/*.S)
INCLUDE_DST  = $(subst src,$(OBJ_DIR)/adainclude,$(INCLUDE_SRC))

DUMMY := $(shell mkdir -p $(OBJ_DIR)/adainclude/asm $(OBJ_DIR)/adalib)

$(OBJ_DIR)/adalib/libgnat.a: $(INCLUDE_DST)
	@$(E) $(COMPONENT) Build "gprbuild --RTS=./obj $(BUILD_OPTS) -P$(COMPONENT)"

$(OBJ_DIR)/adainclude/%.ads: src/%.ads
	cp -a $< $@
$(OBJ_DIR)/adainclude/%.adb: src/%.adb
	cp -a $< $@
$(OBJ_DIR)/adainclude/asm/%.S: src/asm/%.S
	cp -a $< $@

clean:
	@rm -rf $(OBJ_DIR)
