include ../../Makeconf

BUILD_OPTS += --RTS=zfp

all: dumper

dumper:
	gprbuild $(BUILD_OPTS) -P$@

specs: dumper
	$(SKCONFIG) $(OBJ_DIR)/dumper 16#226000#

clean:
	rm -rf $(OBJ_DIR)
