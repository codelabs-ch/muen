include ../../Makeconf

BUILD_OPTS += --RTS=zfp

all: spec

dumper:
	gprbuild $(BUILD_OPTS) -P$@

spec: dumper
	$(SKCONFIG) $(OBJ_DIR)/dumper

clean:
	rm -rf $(OBJ_DIR)
