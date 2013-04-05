include ../../Makeconf

BUILD_OPTS += --RTS=zfp

all: specs

subjects:
	gprbuild $(BUILD_OPTS) -P$@

specs: subjects
	$(SKCONFIG) $(OBJ_DIR)/subject_a 16#236000#
	$(SKCONFIG) $(OBJ_DIR)/subject_b 16#246000#

clean:
	rm -rf obj
