include ../../Makeconf

BUILD_OPTS += --RTS=zfp

all: specs

subjects:
	gprbuild $(BUILD_OPTS) -P$@

specs: subjects
	$(SKCONFIG) $(OBJ_DIR)/subject1
	$(SKCONFIG) $(OBJ_DIR)/subject2

clean:
	rm -rf obj
