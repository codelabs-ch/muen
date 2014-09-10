include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: idle

idle:
	gprbuild $(BUILD_OPTS) -P$@

clean:
	rm -rf $(OBJ_DIR)
