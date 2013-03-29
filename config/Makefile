include ../Makeconf

all: config_subjects

skc_tool:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

config_subjects: skc_tool
	$(OBJ_DIR)/skconfig

clean:
	@rm -rf $(OBJ_DIR) include/skc-*.ads
