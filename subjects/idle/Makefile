include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: idle

idle:
	gprbuild $(BUILD_OPTS) -P$@

specs: idle
	$(SKCONFIG) $(OBJ_DIR)/idle 16#31a000#

clean:
	rm -rf $(OBJ_DIR)
