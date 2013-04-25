include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: dumper

dumper:
	gprbuild $(BUILD_OPTS) -P$@

specs: dumper
	$(SKCONFIG) $(OBJ_DIR)/dumper 16#236000#

clean:
	rm -rf $(OBJ_DIR)
