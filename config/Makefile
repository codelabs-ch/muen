include ../Makeconf

all: skc_tool

skc_tool:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

clean:
	@rm -rf $(OBJ_DIR)
