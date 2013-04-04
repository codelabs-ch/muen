include ../../Makeconf

BUILD_OPTS += --RTS=zfp

all: specs

subjects:
	gprbuild $(BUILD_OPTS) -P$@

specs: subjects
	$(SKCONFIG) $(OBJ_DIR)/subject_a
	$(SKCONFIG) $(OBJ_DIR)/subject_b

clean:
	rm -rf obj
