include ../../Makeconf

BUILD_OPTS += --RTS=zfp

all: idle

idle:
	gprbuild $(BUILD_OPTS) -P$@

spec: idle
	$(SKCONFIG) $(OBJ_DIR)/idle 16#256000#

clean:
	rm -rf $(OBJ_DIR)
