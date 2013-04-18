include ../../Makeconf

BUILD_OPTS += --RTS=zfp

all: idle

idle:
	gprbuild $(BUILD_OPTS) -P$@

specs: idle
	$(SKCONFIG) $(OBJ_DIR)/idle 16#266000#

clean:
	rm -rf $(OBJ_DIR)
