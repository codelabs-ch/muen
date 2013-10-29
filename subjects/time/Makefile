include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: time

time:
	gprbuild $(BUILD_OPTS) -P$@

specs: time
	$(SKCONFIG) $(OBJ_DIR)/time 16#277000#

clean:
	@rm -rf $(OBJ_DIR)
