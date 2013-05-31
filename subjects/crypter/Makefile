include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: crypter

crypter:
	gprbuild $(BUILD_OPTS) -P$@

specs: crypter
	$(SKCONFIG) $(OBJ_DIR)/crypter 16#277000#

clean:
	rm -rf $(OBJ_DIR)
