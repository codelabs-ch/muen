include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: vt

vt:
	gprbuild $(BUILD_OPTS) -P$@

specs: vt
	$(SKCONFIG) $(OBJ_DIR)/vt 16#297000#

clean:
	@rm -rf $(OBJ_DIR)
