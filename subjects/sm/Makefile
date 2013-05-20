include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: sm

sm:
	gprbuild $(BUILD_OPTS) -P$@

specs: sm
	$(SKCONFIG) $(OBJ_DIR)/sm 16#337000#

clean:
	@rm -rf $(OBJ_DIR)
